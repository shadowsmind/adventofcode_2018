lazy val commonSettings = Defaults.coreDefaultSettings ++ Formatting.formatSettings ++ Seq(
  organization := "com.adventofcode",
  version := "1.0",
  scalaVersion := Dependencies.scalaLastVersion,
  scalacOptions ++= List("-unchecked", "-deprecation", "-encoding", "UTF8", "-feature")
)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(
    name := "adventofcode_2018",
    libraryDependencies ++= Dependencies.test
  )
