name := "scala-bootcamp"

version := "0.2"

scalaVersion := "2.13.4"

// From https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
	"-deprecation",
	"-feature",
	"-Ymacro-annotations",
)

val catsVersion = "2.2.0"
val catsTaglessVersion = "0.11"
val catsEffectVersion = "2.2.0"
val catsScalacheckVersion = "0.2.0"


val scalaTestVersion = "3.1.0.0-RC2"

libraryDependencies ++= Seq(
	"org.typelevel" %% "cats-core" % catsVersion,
	"org.typelevel" %% "cats-effect" % catsEffectVersion,
	"com.codecommit" %% "cats-effect-testing-scalatest" % "0.4.1" % Test,
	"io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
	"org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test,
	"org.scalatestplus" %% "selenium-2-45" % scalaTestVersion % Test,
	"org.mockito" %% "mockito-scala" % "1.15.0" % Test,
	"org.typelevel" %% "cats-tagless-macros" % catsTaglessVersion,
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)

run / fork := true
