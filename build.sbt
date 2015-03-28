name := "days"

version := "1.0"

organization := "org.phasanix"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq (
  "org.scalatest" %% "scalatest" % "2.1.6" % "test",
  "com.storm-enroute" %% "scalameter" % "0.6"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
