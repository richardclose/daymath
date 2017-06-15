name := "daymath"

version := "1.1-SNAPSHOT"

organization := "org.phasanix"

crossScalaVersions := Seq("2.11.8", "2.12.2")

scalaVersion := "2.12.2"

libraryDependencies ++= Seq (
  "org.scalatest"     %% "scalatest"  % "3.2.0-SNAP4"   % "test"
)

// testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
