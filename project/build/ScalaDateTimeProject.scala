
import sbt._

class ScalaDateTimeProject( info: ProjectInfo ) extends DefaultProject( info ) {

  val mavenLocal = "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"

  val scalatoolsSnapshot = "Scala Tools Snapshot" at "http://scala-tools.org/repo-snapshots/"
  val scalatoolsRelease  = "Scala Tools Release"  at "http://scala-tools.org/repo-releases/"

  val centralMavenRepo = "central-maven"   at "http://repo1.maven.org/maven2/"

	override def compileOptions = super.compileOptions ++ Seq(Unchecked)

  override def libraryDependencies = Set(
    "org.scalatest" % "scalatest" % "1.3"  % "test",
    "org.testng"    % "testng"    % "5.14" % "test"
  ) ++ super.libraryDependencies

}

