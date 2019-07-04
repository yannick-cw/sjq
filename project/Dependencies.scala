import sbt._

object Versions {
  val circe   = "0.11.0"
  val refined = "0.9.4"
}

object Dependencies {
  val caseApp        = "com.github.alexarchambault" %% "case-app"       % "2.0.0-M6"
  val logbackClassic = "ch.qos.logback"             % "logback-classic" % "1.2.3"
  val scalaTest      = "org.scalatest"              %% "scalatest"      % "3.0.7"

  lazy val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser",
    "io.circe" %% "circe-optics"
  ).map(_ % Versions.circe)

  val dependencies = Seq(
    caseApp,
    logbackClassic,
    scalaTest       % "test",
    "org.scalameta" %% "scalameta" % "4.1.9",
  ) ++ circe
}
