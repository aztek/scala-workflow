import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalacOptions ++= Seq("-feature"),
    scalaVersion := "2.11.0-SNAPSHOT",
    scalaOrganization := "org.scala-lang.macro-paradise",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases")
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("core"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-reflect" % _),
      libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
    )
  )
}
