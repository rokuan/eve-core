name := "EveCore"

version := "1.0"

scalaVersion := "2.11.8"

retrieveManaged := true

retrievePattern := "[artifact].[ext]"

libraryDependencies ++= Seq(
  "org.json4s" % "json4s-native_2.11" % "3.5.0",
  "org.json4s" % "json4s-jackson_2.11" % "3.5.0",
  "org.scalatest" % "scalatest_2.11" % "3.0.1"
)
