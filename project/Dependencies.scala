import sbt._

object Dependencies {

  // @formatter:off
  val deps = Seq(
    "org.typelevel"              %% "cats-core"          % "1.0.1",
    "com.github.wookietreiber"   %% "scala-chart"        % "0.5.1",
    "ch.qos.logback"             %  "logback-classic"     % "1.2.3",
    "com.typesafe.scala-logging" %% "scala-logging"      % "3.9.0",
    "eu.timepit"                 %% "refined"            % "0.9.2",
    "eu.timepit"                 %% "refined-scalacheck" % "0.9.2",

    "org.scalatest"              %% "scalatest"          % "3.0.5"  % Test,
    "org.scalacheck"             %% "scalacheck"         % "1.14.0" % Test
  )
  // @formatter:on
}
