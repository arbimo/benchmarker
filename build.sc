// build.sc
import mill._, scalalib._

import $ivy.`ch.epfl.scala::mill-bloop:1.0.0`


object bench extends ScalaModule {
  def scalaVersion = "2.12.6"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.2.2",
    ivy"com.lihaoyi::upickle:0.7.1",
    ivy"org.typelevel::cats-effect:1.0.0",
    ivy"com.github.alexarchambault::case-app:2.0.0-M5",
    ivy"tech.sparse::toml-scala:0.2.0"
  )
}