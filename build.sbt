lazy val root = (project in file(".")).settings(libraryDependencies ++= Dependencies.deps)

scalacOptions += "-Ypartial-unification"
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)