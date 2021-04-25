name := "task20"

version := "0.1"

scalaVersion := "2.13.4"

// From https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions ++= Seq(
	"-deprecation",
	"-feature",
	"-Ymacro-annotations",
)

val http4sVersion = "0.21.22"
val circeVersion = "0.13.0"
val catsVersion = "2.2.0"
val catsTaglessVersion = "0.11"
val catsEffectVersion = "2.2.0"

libraryDependencies ++= Seq(
	"org.typelevel" %% "cats-core" % catsVersion,
	"org.typelevel" %% "cats-effect" % catsEffectVersion,
	"org.http4s" %% "http4s-dsl" % http4sVersion,
	"org.http4s" %% "http4s-blaze-server" % http4sVersion,
	"org.http4s" %% "http4s-blaze-client" % http4sVersion,
	"org.http4s" %% "http4s-circe" % http4sVersion,
	"org.http4s" %% "http4s-jdk-http-client" % "0.3.6",
	"ch.qos.logback" % "logback-classic" % "1.2.3",
	"io.circe" %% "circe-core" % circeVersion,
	"io.circe" %% "circe-generic" % circeVersion,
	"io.circe" %% "circe-generic-extras" % circeVersion,
	"io.circe" %% "circe-optics" % circeVersion,
	"io.circe" %% "circe-parser" % circeVersion,
	"org.slf4j" % "slf4j-nop" % "1.6.4"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.1" cross CrossVersion.full)

run / fork := true
run / connectInput := true
run / outputStrategy := Some(StdoutOutput)
