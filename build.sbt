lazy val dmspredictor = (project in file("."))
  .settings(
    name                := "DMsPredictor",
    version             := "1.0",
    scalaVersion        := "2.13.8",
    Compile / mainClass := some("io.github.tjheslin1.dmspredictor.Main"),
    assemblyJarName     := "DMsPredictor_full.jar",
    libraryDependencies ++= Seq(
      // format: off
      "org.typelevel"               %% "cats-core"                        % "2.8.0",
      "ch.qos.logback"              %  "logback-classic"                  % "1.4.0",
      "com.typesafe.scala-logging"  %% "scala-logging"                    % "3.9.5",
      "eu.timepit"                  %% "refined"                          % RefinedVersion,
      "eu.timepit"                  %% "refined-scalacheck"               % RefinedVersion,
      "com.github.julien-truffaut"  %% "monocle-core"                     % MonocleVersion,
      "com.github.julien-truffaut"  %% "monocle-macro"                    % MonocleVersion,

      "io.circe"                    %% "circe-core"                       % CirceVersion,
      "io.circe"                    %% "circe-generic"                    % CirceVersion,
      "io.circe"                    %% "circe-parser"                     % CirceVersion,

      "com.amazonaws"               % "aws-java-sdk-dynamodb"             % "1.11.688",
      "com.amazonaws"               % "aws-java-sdk-lambda"               % "1.11.693",
      "com.amazonaws"               % "aws-lambda-java-core"              % "1.2.1",

      "org.scanamo"                 %% "scanamo"                          % "1.0.0-M20",

      "org.scalatest"               %% "scalatest"                        % "3.2.13"         % Test,
      "org.scalatestplus"           %% "scalatestplus-scalacheck"         % "3.1.0.0-RC2"   % Test,

      "com.danielasfregola"         %% "random-data-generator-magnolia"   % "2.9"           % Test,
      "org.scalacheck"              %% "scalacheck"                       % "1.16.0"        % Test
      // format: on
    )
  )

Global / scalacOptions += "-Ymacro-annotations"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val CirceVersion   = "0.14.2"
val MonocleVersion = "2.1.0"
val RefinedVersion = "0.10.1"

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x                             => MergeStrategy.first
}
