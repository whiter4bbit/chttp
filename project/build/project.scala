import sbt._

class chttp(info: ProjectInfo) extends DefaultProject(info) {
   val scalazCore = "org.scalaz" %% "scalaz-core" % "6.0.1" 
   val liftJson = "net.liftweb" %% "lift-json-scalaz" % "2.4-SNAPSHOT"   
   
   lazy val snapshotsRepo = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
}
