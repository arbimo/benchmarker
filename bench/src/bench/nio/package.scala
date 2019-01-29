package bench

import java.io.IOException

import cats._
import cats.implicits._
import scalaz.zio._
import os._

import scala.util.Try


package object nio {

  def walk(path: Path,
            skip: Path => Boolean = _ => false,
            preOrder: Boolean = true,
            followLinks: Boolean = false,
            maxDepth: Int = Int.MaxValue,
            includeTarget: Boolean = false): IO[Nothing, IndexedSeq[Path]] = {
    IO.succeedLazy {
      val res = os.walk(path, skip, preOrder, followLinks, maxDepth, includeTarget)
      println(res)
      res
    }
  }

  def read(path: ReadablePath): IO[IOException, String] = IO.syncCatch {
    os.read(path)
  } {
    case x: IOException => x
  }

  def exists(path: Path): IO[Nothing, Boolean] =
    IO.syncException { os.exists(path) }.redeemPure(_ => false, identity)

  def existsAndContains(path: Path, str: String): IO[Nothing, Boolean] =
    nio.read(path).map(_.contains(str)).redeemPure(_ => false, identity)

}
