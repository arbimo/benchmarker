// build.sc
import mill._
import scalalib._
import $ivy.`ch.epfl.scala::mill-bloop:1.2.5`
import mill.define.Target
import mill.util.Loose


object bench extends ScalaModule {
  def scalaVersion = "2.12.6"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.2.2",
    ivy"com.lihaoyi::upickle:0.7.1",
    ivy"org.typelevel::cats-effect:1.0.0",
    ivy"org.scalaz::scalaz-zio:0.6.0" ,
    ivy"org.scalaz::scalaz-zio-interop-cats:0.6.0",
    ivy"com.github.alexarchambault::case-app:2.0.0-M5",
    ivy"tech.sparse::toml-scala:0.2.0"
  )

  override def compileIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.8")
  override def scalacPluginIvyDeps = Agg(ivy"org.spire-math::kind-projector:0.9.8")

  override def mainClass = Some("bench.Main")
}