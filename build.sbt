name := "Sai"

version := "1.0"

scalaVersion := "2.12.5"

// the project does not reflect the default sbt structure
scalaSource in Compile := baseDirectory.value / "src/sai"
scalaSource in Test := baseDirectory.value / "src/test"

// compile java example classes which are used in the scala tests
unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/artifacts"

libraryDependencies ++= Seq(
  "org.apache.bcel" % "bcel" % "6.2",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
