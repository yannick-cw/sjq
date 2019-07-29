package io.yannick_cw.sjq

import cats.data.{NonEmptyList, State}
import cats.data.State._
import cats.syntax.traverse._
import cats.instances.map.catsKernelStdMonoidForMap
import cats.syntax.foldable._
import cats.instances.list._
import cats.kernel.Semigroup
import io.circe.Json

object jsonToCaseClass {
  case class CC(name: String, content: Map[String, String])
  def apply(j: Json): List[(String, String)] = {
    val allCCs = buildCC(j, "CC").runS(T(List.empty)).value.doneCCs
    val ccs =
      allCCs
        .map(cc => cc.copy(content = cc.content.filter { case (key, value) => key.nonEmpty && value.nonEmpty }))

    ccs.map(cc => (cc.name, renderCC(cc)))
  }

  private def renderCC(cc: CC): String =
    s"case class ${cc.name}(${cc.content.map { case (key, value) => s"$key: $value" }.mkString(", ")})"

  private def addKV(value: String, nextLevelName: String): S =
    pure((cc: CC) => cc.copy(content = cc.content.+((nextLevelName, value))))

  case class T(doneCCs: List[CC])
  type S = State[T, CC => CC]

  private def mergeCCs(nextLevelContent: List[Map[String, String]]) = {
    implicit val sSemi: Semigroup[String] =
      (x: String, y: String) =>
        if (x == "Null" && y != "Null") s"Option[$y]" else if (y == "Null" && x != "Null") s"Option[$x]" else x
    val commonKeys = NonEmptyList
      .fromList(nextLevelContent)
      .map(list => list.tail.foldLeft(list.head.keySet)((commonKeys, next) => commonKeys.intersect(next.keySet)))
      .getOrElse(Set.empty)
    nextLevelContent.combineAll.toList.map {
      case (key, value) if commonKeys.contains(key) => (key, value)
      case (key, value)                             => (key, s"Option[$value]")
    }
  }

  private def mergeNextLevelsCCs(nextLevelName: String, t: T) = {
    val nextLevelsCcs = t.doneCCs
      .filter(_.name == nextLevelName)
      .map(_.content)
    val mergedCCs = mergeCCs(nextLevelsCcs)
    t.copy(
      doneCCs =
        (if (mergedCCs.nonEmpty) List(CC(nextLevelName, mergedCCs.toMap))
         else List.empty) ::: t.doneCCs.filterNot(cc => cc.name == nextLevelName))
  }

  private def buildNextLevelType(allNextLevelTypes: Option[NonEmptyList[String]]) = {
    allNextLevelTypes
      .map(
        nel =>
          if (nel.forall(_ == nel.head)) nel.head
          else "Json")
      .getOrElse("String")
  }

  private def buildCC(j: Json, nextLevelName: String): S = j.fold(
    addKV("Null", nextLevelName),
    _ => addKV("Boolean", nextLevelName),
    _ => addKV("Double", nextLevelName),
    _ => addKV("String", nextLevelName),
    array =>
      for {
        ccModifications <- array.toList.traverse(buildCC(_, nextLevelName))
        _               <- modify[T](mergeNextLevelsCCs(nextLevelName, _))
        value = ccModifications.map(f => f(CC("", Map.empty)))
        allNextLevelTypes = NonEmptyList.fromList(
          value.flatMap(_.content.filter(tuple => nextLevelName.startsWith(tuple._1)).values))
        nextLevelTypeName = buildNextLevelType(allNextLevelTypes)
      } yield
        (cc: CC) => cc.copy(content = cc.content.+((cleanAddedField(nextLevelName), s"List[${nextLevelTypeName}]"))),
    jObj => {
      for {
        currentState <- get[T]
        ccModifications <- jObj.toMap.toList.traverse {
          case (key, value) =>
            val safeNextLevelName = findFreeName(currentState.doneCCs, key)
            buildCC(value, safeNextLevelName)
        }
        updatedCC = ccModifications.foldLeft(CC(nextLevelName, Map.empty))((cc, ccOp) => ccOp(cc))
        _ <- modify[T](t => t.copy(doneCCs = updatedCC :: t.doneCCs))
      } yield
        (cc: CC) =>
          cc.copy(content = cc.content.+((cleanAddedField(nextLevelName), nextLevelName))) // ignored in first run
    }
  )

  @scala.annotation.tailrec
  private def findFreeName(ccs: List[CC], name: String): String =
    if (ccs.exists(_.name == name)) findFreeName(ccs, name + "1") else name

  private def cleanAddedField(nextLevelName: String): String = nextLevelName.reverse.dropWhile(_ == '1').reverse
}
