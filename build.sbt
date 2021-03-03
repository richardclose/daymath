name := "daymath"

version := "1.0.2-SNAPSHOT"

organization := "org.phasanix"

scalaVersion := "2.13.5"

libraryDependencies ++= Seq (
  "org.scalatest"     %% "scalatest"  % "3.2.5"   % "test"
)

// testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := false
