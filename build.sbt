import Dependencies._

lazy val root = (project in file(".")).settings(
  libraryDependencies ++= scalaLogging ++ Seq(
    scalatest,
    cats,
    scalaChart
  ))

scalacOptions += "-Ypartial-unification"
