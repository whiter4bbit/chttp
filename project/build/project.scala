import sbt._

class chttp(info: ProjectInfo) extends DefaultProject(info) {
   val scalazCore = "org.scalaz" %% "scalaz-core" % "6.0.1" 
}
