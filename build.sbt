lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      // @formatter:off
      "org.typelevel"               %% "cats-core"                        % "1.0.1",
      "com.github.wookietreiber"    %% "scala-chart"                      % "0.5.1",
      "ch.qos.logback"              %  "logback-classic"                  % "1.2.3",
      "com.typesafe.scala-logging"  %% "scala-logging"                    % "3.9.0",
      "eu.timepit"                  %% "refined"                          % "0.9.2",
      "eu.timepit"                  %% "refined-scalacheck"               % "0.9.2",
      "com.github.julien-truffaut"  %% "monocle-core"                     % MonocleVersion,
      "com.github.julien-truffaut"  %% "monocle-macro"                    % MonocleVersion,

      "com.danielasfregola"         %% "random-data-generator-magnolia"   % "2.6"     % Test,
      "org.scalatest"               %% "scalatest"                        % "3.0.5"   % Test,
      "org.scalacheck"              %% "scalacheck"                       % "1.14.0"  % Test
      //@formatter:on
    ))

scalacOptions += "-Ypartial-unification"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val MonocleVersion = "1.5.0"
