name := "learn_algrithm"

version := "0.1"

scalaVersion := "2.13.5"
scalacOptions ++= Seq("-language:implicitConversions", "-deprecation")
//libraryDependencies += "org.scalameta" %% "munit" % "0.7.22" % Test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
