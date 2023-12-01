ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.2"
lazy val root = (project in file("."))
  .settings(
    name := "advent-2023"
  )
