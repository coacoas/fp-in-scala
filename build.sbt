name := "fp-in-scala"
scalaVersion := "2.12.3"

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions += "-Ypartial-unification"
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

lazy val catsVersion = "1.0.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-macros",
  "org.typelevel" %% "cats-kernel",
  "org.typelevel" %% "cats-core",
  "org.typelevel" %% "cats-free"
).map(_ % catsVersion)

libraryDependencies +=  "org.typelevel" %% "cats-effect" % "0.8"
libraryDependencies +=  "org.scalatest" %% "scalatest" % "3.0.3" % "test"

