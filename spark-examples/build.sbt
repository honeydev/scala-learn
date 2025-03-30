ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "spark-examples"
  )


libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.5.5"


