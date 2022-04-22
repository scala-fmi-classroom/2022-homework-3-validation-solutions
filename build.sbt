name := "validation"
version := "0.1"

scalaVersion := "3.1.1"

libraryDependencies ++= Seq(
  "org.mindrot" % "jbcrypt" % "0.4",
  "org.scalatest" %% "scalatest" % "3.2.11" % Test
)

scalacOptions ++= Seq(
  "-new-syntax",
  "-indent"
)
