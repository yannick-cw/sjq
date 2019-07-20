package io.yannick_cw.sjq

import io.circe.Json
import cats.implicits._

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

  private def buildCC(j: Json, nextLevelName: String, doneCCs: List[CC]): (String, List[CC]) = {
    j.fold(
      // todo decode not at all if null
      "Option[String]" -> doneCCs,
      _ => "Boolean" -> doneCCs,
      _ => "Double"  -> doneCCs,
      _ => "String"  -> doneCCs,
      array =>
        array.toList match {
          case all @ ele :: rest if all.forall(_.isObject) =>
            val innerType =
              rest.foldLeft(buildCC(ele, nextLevelName, doneCCs)) {
                case ((newValueName, aggCCs), nextJson) =>
                  newValueName -> buildCC(nextJson, nextLevelName, aggCCs)._2
              }

            val (valueName, ccs) = innerType match {
              case (valueName, Nil)        => valueName -> doneCCs
              case (valueName, ele :: Nil) => valueName -> (ele :: Nil)
              case (valueName, ele :: more) =>
                val (nextLevelCCs, otherCCs) = (ele :: more)
                  .partition(_.name.startsWith(valueName))

                val allContent = nextLevelCCs.map(_.content).fold(Map.empty)(_ ++ _)
                val keyCountPerContent =
                  nextLevelCCs.map(cc => cc.content.map[String, Int] { case (key, _) => (key, 1) }).combineAll

                val keyCount = nextLevelCCs.size

                val newCC = ele.copy(content = keyCountPerContent.map({
                  case (key, count) if count == keyCount => key -> allContent(key)
                  case (key, _)                          => key -> s"Option[${allContent(key)}]"
                }))
                valueName -> (newCC :: otherCCs)
            }

            s"List[$valueName]" -> ccs
          case ele :: rest if rest.forall(_ == ele) =>
            val (newValue, allCCs) = buildCC(ele, nextLevelName, doneCCs)
            s"List[$newValue]" -> allCCs
          case _ => "List[String]" -> doneCCs
      },
      jObj => {
        val (allNewCCs, newCC) = jObj.toMap.foldLeft(doneCCs -> CC(nextLevelName, Map.empty)) {
          case ((allCCs, currCC), (key, value)) =>
            val safeNextLevelName     = findFreeName(currCC :: allCCs, key)
            val (newValue, allNewCCs) = buildCC(value, safeNextLevelName, allCCs)
            (allNewCCs, currCC.copy(content = currCC.content.updated(key, newValue)))
        }
        (nextLevelName, newCC :: allNewCCs)
      }
    )
  }

  private def findFreeName(ccs: List[CC], name: String): String =
    if (ccs.exists(_.name == name)) findFreeName(ccs, name + "1") else name
}
