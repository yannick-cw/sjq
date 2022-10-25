import Dependencies._

name := "sjq"

organization in ThisBuild := "yannick-cw"
maintainer := "yannick.gladow@gmail.com"
scalaVersion in ThisBuild := "2.13.10"
scalafmtVersion in ThisBuild := "1.2.0"
scalafmtOnCompile in ThisBuild := true

lazy val cli = project
  .in(file("."))
  .settings(
    libraryDependencies ++= dependencies,
    Defaults.itSettings
  )
  .configs(IntegrationTest)
  .enablePlugins(JavaAppPackaging)
