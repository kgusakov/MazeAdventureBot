import sbt.{Resolver, Def}
import sbt.Keys._

object Commons {

  val settings: Seq[Def.Setting[_]] = Seq(
    version := "1.0",
    resolvers += Resolver.jcenterRepo,
    scalaVersion := "2.11.8"
  )
}