package io.yannick_cw.sjq

import cats.instances.map.catsKernelStdMonoidForMap
import cats.instances.int.catsKernelStdGroupForInt
import cats.syntax.foldable._
import cats.instances.list._
import cats.kernel.Semigroup
import io.circe.Json

object jsonToCaseClass {
  case class CC(name: String, content: Map[String, String])
  def apply(j: Json): List[(String, String)] = {
    val (_, allCCs) = buildCC(j, "CC", List.empty)
    val ccs =
      allCCs
        .map(cc => cc.copy(content = cc.content.filter { case (key, value) => key.nonEmpty && value.nonEmpty }))

    ccs.map(cc => (cc.name, renderCC(cc)))
  }

  private def renderCC(cc: CC): String =
    s"case class ${cc.name}(${cc.content.map { case (key, value) => s"$key: $value" }.mkString(", ")})"

  private def buildCC(j: Json, nextLevelName: String, doneCCs: List[CC]): (Option[String], List[CC]) = {
    j.fold(
      Some("Null") -> doneCCs,
      _ => Some("Boolean") -> doneCCs,
      _ => Some("Double")  -> doneCCs,
      _ => Some("String")  -> doneCCs,
      array =>
        array.toList match {
          case all @ ele :: rest if all.forall(_.isObject) =>
            val innerType =
              rest.foldLeft(buildCC(ele, nextLevelName, doneCCs)) {
                case ((newValueName, aggCCs), nextJson) =>
                  newValueName -> buildCC(nextJson, nextLevelName, aggCCs)._2
              }
            val (valueName, ccs) = innerType match {
              case (Some(valueName), Nil)        => valueName -> doneCCs
              case (Some(valueName), ele :: Nil) => valueName -> (ele :: Nil)
              case (Some(valueName), ele :: more) =>
                val (nextLevelCCs, otherCCs) = (ele :: more)
                  .partition(_.name.startsWith(valueName))

                implicit val sSemi: Semigroup[String] =
                  (x: String, y: String) => if (x == "Null") s"Option[$y]" else if (y == "Null") s"Option[$x]" else x
                val allContent = nextLevelCCs.map(_.content).combineAll
                val keyCountPerContent =
                  nextLevelCCs.map(cc => cc.content.map[String, Int] { case (key, _) => (key, 1) }).combineAll

                val keyCount = nextLevelCCs.size

                val newCC = ele.copy(content = keyCountPerContent.map({
                  case (key, count) if count == keyCount => key -> allContent(key)
                  case (key, _)                          => key -> s"Option[${allContent(key)}]"
                }))
                valueName -> (newCC :: otherCCs)
              case _ => "" -> doneCCs
            }

            Some(s"List[$valueName]") -> ccs
          case ele :: _ =>
            val (newValue, allCCs) = buildCC(ele, nextLevelName, doneCCs)
            newValue.map(v => s"List[$v]") -> allCCs
            // todo fix if things have different types
          case _ => Some("List[String]") -> doneCCs
      },
      jObj => {
        val (allNewCCs, newCC) = jObj.toMap.foldLeft(doneCCs -> CC(nextLevelName, Map.empty)) {
          case ((allCCs, currCC), (key, value)) =>
            val safeNextLevelName     = findFreeName(currCC :: allCCs, key)
            val (newValue, allNewCCs) = buildCC(value, safeNextLevelName, allCCs)
            allNewCCs -> newValue.map(v => currCC.copy(content = currCC.content.updated(key, v))).getOrElse(currCC)
        }
        (Some(nextLevelName), newCC :: allNewCCs)
      }
    )
  }

  private def findFreeName(ccs: List[CC], name: String): String =
    if (ccs.exists(_.name == name)) findFreeName(ccs, name + "1") else name
}
