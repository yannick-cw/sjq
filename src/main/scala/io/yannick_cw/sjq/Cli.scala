package io.yannick_cw.sjq
import caseapp._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

case class CliArgs(@ExtraName("a")
                   access: String,
                   @ExtraName("j")
                   json: Option[String])

object Cli extends CaseApp[CliArgs] {

  private def readIn =
    Try(Await.result(Future(scala.io.StdIn.readLine()), 10.millis)).toEither

  override def run(options: CliArgs, remainingArgs: RemainingArgs): Unit = {
    val res = for {
      json <- options.json
        .fold(readIn)(Right(_))
        .left
        .map(_ => new Exception("Neither --json argument or json piped | was given"))
      result <- executeAccessPattern.generateFun(options.access, json)
    } yield result

    println(res.fold(_.getMessage, x => x))
  }
}

import io.circe.Json
import io.circe.parser.parse

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox
import scala.util.Try

object executeAccessPattern {
  private def buildCode(access: String, ccs: String) =
    s"""
       | import io.circe.Json
       | 
       | (json: Json) => {
       |  import io.circe.generic.auto._
       |  import io.circe._
       |  import io.circe.syntax._
       |
       |  $ccs
       |
       |  val root = json.as[CC].getOrElse(null)
       |  val result = $access
       |  result.asJson.spaces2
       |}
      """.stripMargin

  private val cm = universe.runtimeMirror(getClass.getClassLoader)
  private val tb = cm.mkToolBox()

  def generateFun(access: String, json: String): Either[Throwable, String] = {
    for {
      parsedJson <- parse(json)
      caseClasses = jsonToCaseClass(parsedJson)
      caseClassesWithReflection = caseClasses
        .map { case (name, cc) => cc + s"\nscala.reflect.classTag[$name].runtimeClass" }
        .mkString("\n")
      code = buildCode(access, caseClassesWithReflection)
      tree   <- Try(tb.parse(code)).toEither
      result <- Try(tb.eval(tree).asInstanceOf[Json => String](parsedJson)).toEither
    } yield result
  }
}

object jsonToCaseClass {
  case class CC(name: String, content: Map[String, String])
  def apply(j: Json): List[(String, String)] = {
    val (topCC, moreCCs) = buildCC(j, s => CC("CC", Map(s -> "")), List.empty)
    val ccs              = (topCC :: moreCCs).map(cc => cc.copy(content = cc.content.filterKeys(_.nonEmpty)))
    ccs.map(cc => (cc.name, renderCC(cc)))
  }

  private def renderCC(cc: CC): String =
    s"case class ${cc.name}(${cc.content.map { case (key, value) => s"$key: $value" }.mkString(", ")})"

  private def buildCC(j: Json, activeCaseClass: String => CC, doneCCs: List[CC]): (CC, List[CC]) = {
    j.fold(
      activeCaseClass("null") -> doneCCs,
      _ => activeCaseClass("Boolean") -> doneCCs,
      _ => activeCaseClass("Double")  -> doneCCs,
      _ => activeCaseClass("String")  -> doneCCs,
      array => {
        val outter = (s: String) => activeCaseClass(s"List[$s]")
        array.headOption
          .map(ele => buildCC(ele, outter, List.empty))
          .getOrElse(outter("Any") -> List.empty)
      },
      jObj =>
        jObj.toMap.foldLeft((s: String) => activeCaseClass(s) -> List.empty[CC])((activeCC, keyVal) =>
          s => {
            if (keyVal._2.isObject) {
              val (newCC, _)     = activeCC(keyVal._1)
              val withRefToNewCC = newCC.copy(content = newCC.content.updated(keyVal._1, keyVal._1))
              val (cc, all) =
                buildCC(keyVal._2, s => CC(keyVal._1, Map(s -> "")), List.empty)
              (withRefToNewCC.copy(content = withRefToNewCC.content.updated(s, "")), cc :: all)
            } else {
              val (newCC, _) = activeCC(keyVal._1)
              val recCC      = (s: String) => newCC.copy(content = newCC.content.updated(keyVal._1, s))
              val (cc, all)  = buildCC(keyVal._2, recCC, List.empty)
              (cc.copy(content = cc.content.updated(s, "")), all)
            }
        })("")
    )
  }
}
