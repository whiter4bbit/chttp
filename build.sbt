name := "chttp"

version := "1.0"

organization := "info.whiter4bbit"

libraryDependencies ++= Seq(
   "org.scalaz" %% "scalaz-core" % "6.0.1",
   "net.liftweb" %% "lift-json-scalaz" % "2.4-SNAPSHOT"
)

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

scalaVersion := "2.9.0-1"
