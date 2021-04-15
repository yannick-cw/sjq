import sbt._

object Versions {
  val circe         = "0.12.0-RC2"
  val caseApp       = "2.0.5"
  val scalaTest     = "3.0.8"
  val scalaCompiler = "2.13.0"
  val scalaCheck    = "1.14.0"
}

object Dependencies {
  val caseApp       = "com.github.alexarchambault"   %% "case-app"   % Versions.caseApp
  val scalaTest     = "org.scalatest"                %% "scalatest"  % Versions.scalaTest
  val scalaCheck    = "org.scalacheck"               %% "scalacheck" % Versions.scalaCheck % "it"

  val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser",
  ).map(_ % Versions.circe) :+ "io.circe" %% "circe-testing" % Versions.circe % "it"

  val scalaCompiler = "org.scala-lang" % "scala-compiler" % Versions.scalaCompiler

  val dependencies = Seq(
    caseApp,
    scalaTest     % "test,it",
    scalaCompiler,
    scalaCheck
  ) ++ circe
}
