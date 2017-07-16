name := "daymath"

version := "1.0.2-SNAPSHOT"

organization := "org.phasanix"

crossScalaVersions := Seq("2.11.11", "2.12.2")

scalaVersion := "2.12.2"

libraryDependencies ++= Seq (
  "org.scalatest"     %% "scalatest"  % "3.2.0-SNAP7"   % "test"
)

// testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
