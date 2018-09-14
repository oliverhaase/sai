name := "Sai"

version := "1.0"

scalaVersion := "2.12.5"

// the project does not reflect the default sbt structure
scalaSource in Compile := baseDirectory.value / "src/sai"
scalaSource in Test := baseDirectory.value / "src/test"

// show full warning information
scalacOptions ++= Seq("-unchecked",
                      "-deprecation",
                      "-feature",
                      "-language:implicitConversions",
                      "-Ywarn-unused:-patvars")

// compile java example classes which are used in the scala tests
unmanagedSourceDirectories in Test += baseDirectory.value / "src/test/artifacts"

libraryDependencies ++= Seq(
  "org.apache.bcel" % "bcel"       % "6.2",
  "org.scalatest"   %% "scalatest" % "3.0.5" % "test",
  "org.graphstream" % "gs-core"    % "1.3",
  "org.graphstream" % "gs-ui"      % "1.3"
)

offline := true
