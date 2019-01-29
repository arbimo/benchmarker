package bench

import SolveStatus.{Memout, SAT, Timeout, UNSAT, Unknown}

import scala.concurrent.duration.Duration
import cats._
import cats.implicits._
import scalaz.zio._
import scalaz.zio.console._

object instances {

  implicit val showDuration = new Show[Duration] {
    override def show(t: Duration): String =
      if(t.isFinite()) {
        val secs = t.toUnit(scala.concurrent.duration.SECONDS)
        "%1.2f".format(secs)
      }
      else Symbols.infty
  }
}

object Symbols {
  val SAT = "SAT"
  val UNSAT = "UNSAT"
  val infty = "∞"
  val ko = "-"
}

import instances._

object Tabulator {
  def format(table: Seq[Seq[Any]]) = table match {
    case Seq() => ""
    case _ =>
      val sizes = for (row <- table) yield (for (cell <- row) yield if (cell == null) 0 else cell.toString.length)
      val colSizes = for (col <- sizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatRows(rowSeparator: String, rows: Seq[String]): String = (
    rowSeparator ::
      rows.head ::
      rowSeparator ::
      rows.tail.toList :::
      rowSeparator ::
      List()).mkString("\n")

  def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
    val cells = for ((item, size) <- row.zip(colSizes)) yield if (size == 0) "" else ("%" + size + "s").format(item)
    cells.mkString("|", "|", "|")
  }

  def rowSeparator(colSizes: Seq[Int]) = colSizes map { "-" * _ } mkString("+", "+", "+")
}

sealed trait Aggregate[A, K1, K2, V] {
  def firstKey(a: A): K1
  def secondKey(a: A): K2

  def agg(vs: Iterable[A]): V

  def of(k1: K1, k2: K2, vs: Iterable[A]): V = agg(vs.filter(a => firstKey(a) == k1 && secondKey(a) == k2))
}

object Stats {
  type Solver = String

  object SolvedPerDomain extends Aggregate[RunResult, SolverConf, Domain, Int] {
    override def firstKey(a: RunResult): SolverConf = a.solverConf
    override def secondKey(a: RunResult): Domain = a.pb.domain.domain
    override def agg(vs: Iterable[RunResult]): Int = vs.count(_.status.solved)
  }
  object Solved extends Aggregate[RunResult, SolverConf, Instance, String] {
    override def firstKey(a: RunResult) = a.solverConf
    override def secondKey(a: RunResult) = a.instance
    override def agg(vs: Iterable[RunResult]): String = vs.toSeq match {
      case Seq(a) => a.status match {
        case SAT => Symbols.SAT
        case UNSAT => Symbols.UNSAT
        case Timeout => "TO"
        case Memout => "MO"
        case Unknown => "??"
      }
      case _ => ???
    }
  }
  object Runtime extends Aggregate[RunResult, SolverConf, Instance, Duration] {
    override def firstKey(a: RunResult): SolverConf = a.solverConf
    override def secondKey(a: RunResult) = a.instance
    override def agg(vs: Iterable[RunResult]): Duration = vs.toSeq match {
      case Seq(a) if a.status.solved => a.time
      case Seq(a) => Duration.Inf
      case _ => ???
    }
  }
  object Costs extends Aggregate[RunResult, SolverConf, Instance, Double] {
    override def firstKey(a: RunResult): SolverConf = a.solverConf
    override def secondKey(a: RunResult) = a.instance
    override def agg(vs: Iterable[RunResult]): Double = vs.toSeq match {
      case Seq(a) => a.cost match {
        case Some(x) => x
        case None => Double.NaN
      }
      case _ => ???
    }
  }

  def tabView[K1, K2, V: Show](runs: Iterable[RunResult], agg: Aggregate[RunResult, K1, K2, V]) = {
    val common = commonRuns(runs)

    val keys1 = common.map(agg.firstKey).toSeq.distinct.sortBy(_.toString)
    val keys2 = common.map(agg.secondKey).toSeq.distinct.sortBy(_.toString)
    val tab =

      ("" +: keys1) +:
      (for(d <- keys2) yield {
        d +: (for(s <- keys1) yield {
          agg.of(s, d, common).show
      })
    })
    Tabulator.format(tab)
  }
  case class SolverConf(s: Solver, probVariant: String) {
    override def toString: Solver = s"$s / $probVariant"
  }

  def commonRuns(runs: Iterable[RunResult]) = {
    val problemsByPlanner =
      runs
        .groupBy(_.solverConf)
        .mapValues(rs => rs.map(_.instance).toSet)
    val commonProblems =
      runs.map(_.instance).toSet.filter(pb => problemsByPlanner.values.forall(_.contains(pb)))

    val sharedRuns = runs.filter(r => commonProblems.contains(r.instance))

    sharedRuns
  }



  def print(runs: Iterable[RunResult]): IO[Nothing, Unit] = {
    val com = commonRuns(runs).toSet

    for {
      _ <- putStrLn(tabView(com, Solved))
      _ <- putStrLn(tabView(com, Runtime))
      _ <- putStrLn(tabView(com, Costs))
      _ <-putStrLn(tabView(com, SolvedPerDomain))
    } yield ()

  }



}
