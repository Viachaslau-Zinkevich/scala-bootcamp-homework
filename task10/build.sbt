name := "task10"

version := "0.1"

scalaVersion := "2.13.4"

// From https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
	"-deprecation",
	"-feature",
	"-Ymacro-annotations",
)

val circeVersion = "0.13.0"
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
	"io.circe" %% "circe-core" % circeVersion,
	"io.circe" %% "circe-generic" % circeVersion,
	"io.circe" %% "circe-generic-extras" % circeVersion,
	"io.circe" %% "circe-optics" % circeVersion,
	"io.circe" %% "circe-parser" % circeVersion,
	"org.mockito" %% "mockito-scala" % "1.15.0" % Test,
	"org.scalaj" %% "scalaj-http" % "2.4.2" % Test,
	"org.typelevel" %% "cats-tagless-macros" % catsTaglessVersion,
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)

run / fork := true
