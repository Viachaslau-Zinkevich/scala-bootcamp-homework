name := "task1"

version := "0.1"

scalaVersion := "2.13.4"

val scalaTestVersion = "3.2.2"

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % scalaTestVersion % Test,
	"org.scalatest" %% "scalatest-flatspec" % scalaTestVersion % Test,
	"org.scalactic" %% "scalactic" % scalaTestVersion % Test,
)
