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
      result <- executeAccessPattern(options.access, json)
    } yield result

    println(res.fold(_.getMessage, x => x))
  }
}
