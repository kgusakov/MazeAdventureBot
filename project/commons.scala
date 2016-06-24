import sbt.{Resolver, Def}
import sbt._
import sbt.Keys._

object Commons {

  object Versions {
    val akkaVersion = "2.4.7"
  }

  val settings: Seq[Def.Setting[_]] = Seq(
    version := "1.0",
    libraryDependencies ++= Seq(
      "ch.qos.logback" %  "logback-classic" % "1.1.7",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
      "org.scalatest" %% "scalatest" % "2.2.6" % "test"),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots"),
      Resolver.bintrayRepo("scalaz", "releases"),
      Resolver.bintrayRepo("megamsys", "scala")),
      scalaVersion := "2.11.8"
  )
}