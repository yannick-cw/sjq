package io.yannick_cw.sjq

import io.circe.Json
import cats.implicits._

object jsonToCaseClass {
  case class CC(name: String, content: Map[String, String])
  def apply(j: Json): List[(String, String)] = {
    val (_, allCCs) = buildCC(j, "CC", List.empty)
    val ccs =
      allCCs
        // todo this is just a hack to get around duplicated nested objects
        .distinctBy(_.name)
        .map(cc => cc.copy(content = cc.content.filter { case (key, value) => key.nonEmpty && value.nonEmpty }))

    ccs.map(cc => (cc.name, renderCC(cc)))
  }

  private def renderCC(cc: CC): String =
    s"case class ${cc.name}(${cc.content.map { case (key, value) => s"$key: $value" }.mkString(", ")})"

  private def buildCC(j: Json, nextLevelName: String, doneCCs: List[CC]): (Option[String], List[CC]) = {
    j.fold(
      Some("Option[Any]") -> doneCCs,
      _ => Some("Boolean") -> doneCCs,
      _ => Some("Double")  -> doneCCs,
      _ => Some("String")  -> doneCCs,
      array => {
        val innerType = array.flatMap { ele =>
          val (x, y) = buildCC(ele, nextLevelName, List.empty)
          x.map(_ -> y)
        }
        innerType
          .foldLeft[Option[(String, List[CC])]](None)((left, right) =>
            left.map { case (valueName, ccs) => (valueName, right._2 ++ ccs) }.orElse(Some(right)))
          .map {
            case (valueName, Nil)        => valueName -> doneCCs
            case (valueName, ele :: Nil) => valueName -> (ele :: doneCCs)
            case (valueName, ele :: more) =>
              val (nextLevelCCs, deeperCCs) = (ele :: more)
                .partition(_.name == valueName)

              val allContent = nextLevelCCs.map(_.content).fold(Map.empty)(_ ++ _)
              val keyCountPerContent =
                nextLevelCCs.map(cc => cc.content.map[String, Int] { case (key, _) => (key, 1) }).combineAll

              val keyCount = keyCountPerContent.size

              val newCC = ele.copy(content = keyCountPerContent.map({
                case (key, count) if count == keyCount => key -> allContent(key)
                case (key, _)                          => key -> s"Option[${allContent(key)}]"
              }))

              valueName -> (newCC :: deeperCCs ::: doneCCs)
          }
          .map { case (valueName, ccs) => Some(s"List[$valueName]") -> ccs }
          .getOrElse(Some("List[String]") -> doneCCs)
      },
      jObj => {
        val (allNewCCs, newCC) = jObj.toMap.foldLeft(List.empty[CC] -> CC(nextLevelName, Map.empty)) {
          case ((allCss, currCC), (key, value)) =>
            val (newValue, allNewCCs) = buildCC(value, key, List.empty)
            (allCss ++ allNewCCs,
             newValue.map(newV => currCC.copy(content = currCC.content.updated(key, newV))).getOrElse(currCC))
        }
        (Some(nextLevelName), newCC :: allNewCCs)
      }
    )
  }
}
