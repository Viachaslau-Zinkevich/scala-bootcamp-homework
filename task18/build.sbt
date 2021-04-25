name := "task18"

version := "0.1"

scalaVersion := "2.13.4"

// From https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
	"-deprecation",
	"-feature",
	"-Ymacro-annotations",
)

val catsVersion = "2.2.0"
val catsTaglessVersion = "0.11"
val catsEffectVersion = "2.4.1"

libraryDependencies ++= Seq(
	"org.typelevel" %% "cats-core" % catsVersion,
	"org.typelevel" %% "cats-effect" % catsEffectVersion,
	"org.typelevel" %% "cats-tagless-macros" % catsTaglessVersion,
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)

run / fork := true
