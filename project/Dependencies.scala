import sbt._

object Dependencies {

  val scalaLastVersion      = "2.12.8"

  val scalaTestVersion      = "3.0.5"
  val scalacheckVersion     = "1.14.0"
  val scalaMeterVersion     = "0.10.1"
  val scalaMockVersion      = "3.6.0"

  val test: Seq[ModuleID] = Seq(
    "org.scalatest"      %% "scalatest"                   % scalaTestVersion,
    "org.scalacheck"     %% "scalacheck"                  % scalacheckVersion,
    "com.storm-enroute"  %% "scalameter"                  % scalaMeterVersion,
    "org.scalactic"      %% "scalactic"                   % scalaTestVersion,
    "org.scalamock"      %% "scalamock-scalatest-support" % scalaMockVersion
  ).map(_ % Test)

}
