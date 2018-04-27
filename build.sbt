name := "Sai"

version := "1.0"

scalaVersion := "2.12.5"

scalaSource in Compile := baseDirectory.value / "src/sai"

libraryDependencies += "org.apache.bcel" % "bcel" % "6.2"
