package io.yannick_cw.sjq

import io.circe.Json
import io.circe.parser.parse

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox
import scala.util.Try

object executeAccessPattern {
  private def buildCode(access: String, ccs: String) =
    s"""
       | import io.circe.{Encoder, Decoder, HCursor, Json}
       | implicit val dec: Decoder[Null] = (c: HCursor) => Right(null)
       | implicit val enc: Encoder[Null] = (_: Null) => Json.Null
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

  def apply(access: String, json: String): Either[Throwable, String] = {
    for {
      parsedJson <- parse(json)
      caseClasses = jsonToCaseClass(parsedJson)
      caseClassesWithReflection = caseClasses
        .map { case (name, cc) => cc + s"\nscala.reflect.classTag[$name].runtimeClass" }
        .mkString("\n")
      code = buildCode(access, caseClassesWithReflection)
      tree <- Try(tb.parse(code)).toEither
      result <- Try(tb.eval(tree).asInstanceOf[Json => String](parsedJson)).toEither
    } yield result
  }
}
