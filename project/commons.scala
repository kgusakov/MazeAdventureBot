import sbt.{Resolver, Def}
import sbt._
import sbt.Keys._

object Commons {

  val settings: Seq[Def.Setting[_]] = Seq(
    version := "1.0",
    libraryDependencies ++= Seq(
      "ch.qos.logback" %  "logback-classic" % "1.1.7",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"),
    resolvers ++= Seq(
      Resolver.jcenterRepo,
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots"),
      Resolver.bintrayRepo("scalaz", "releases"),
      Resolver.bintrayRepo("megamsys", "scala")),
      scalaVersion := "2.11.6"
  )
}