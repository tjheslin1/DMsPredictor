lazy val dmspredictor = (project in file("."))
  .settings(
    name := "DMsPredictor",
    version := "1.0",
    scalaVersion := "2.12.8",
    mainClass in Compile := some("io.github.tjheslin1.dmspredictor.Main"),
    assemblyJarName := "DMsPredictor_full.jar",
    libraryDependencies ++= Seq(
      // format: off
      "org.typelevel"               %% "cats-core"                        % "2.1.1",
      "com.github.wookietreiber"    %% "scala-chart"                      % "0.5.1",
      "ch.qos.logback"              %  "logback-classic"                  % "1.2.3",
      "com.typesafe.scala-logging"  %% "scala-logging"                    % "3.9.2",
      "eu.timepit"                  %% "refined"                          % RefinedVersion,
      "eu.timepit"                  %% "refined-scalacheck"               % RefinedVersion,
      "com.github.julien-truffaut"  %% "monocle-core"                     % MonocleVersion,
      "com.github.julien-truffaut"  %% "monocle-macro"                    % MonocleVersion,
      
      "io.circe"                    %% "circe-core"                       % CirceVersion,
      "io.circe"                    %% "circe-generic"                    % CirceVersion,
      "io.circe"                    %% "circe-parser"                     % CirceVersion,

      "com.amazonaws"               % "aws-java-sdk-dynamodb"             % "1.11.688",
      "com.amazonaws"               % "aws-java-sdk-lambda"               % "1.11.693",
      "com.amazonaws"               % "aws-lambda-java-core"              % "1.2.0",
      
      "com.gu"                      %% "scanamo"                          % "1.0.0-M8",

      "org.scalatest"               %% "scalatest"                        % "3.1.1"         % Test,
      "org.scalatestplus"           %% "scalatestplus-scalacheck"         % "3.1.0.0-RC2"   % Test,

      "com.danielasfregola"         %% "random-data-generator-magnolia"   % "2.8"           % Test,
      "org.scalacheck"              %% "scalacheck"                       % "1.14.3"        % Test
      // format: on
    ),
    autoCompilerPlugins := true,
    addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.full)
  )

scalacOptions += "-Ypartial-unification"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val CirceVersion   = "0.13.0"
val MonocleVersion = "2.0.2"
val RefinedVersion = "0.9.13"

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x                             => MergeStrategy.first
}
