import Dependencies._

name := "sjq"

(ThisBuild / organization) := "yannick-cw"
maintainer := "yannick.gladow@gmail.com"
(ThisBuild / scalaVersion) := "2.13.0"
(ThisBuild / scalafmtVersion) := "1.2.0"
(ThisBuild / scalafmtOnCompile) := true

lazy val cli = project
  .in(file("."))
  .settings(
    libraryDependencies ++= dependencies,
    Defaults.itSettings
  )
  .configs(IntegrationTest)
  .enablePlugins(JavaAppPackaging)
