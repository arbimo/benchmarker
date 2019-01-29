package bench

import SolveStatus.{Memout, SAT, Timeout, UNSAT, Unknown}
import cats._
import cats.implicits._
import scalaz.zio._
import scalaz.zio.interop.catz._
import os.{Path, RelPath}

import scala.concurrent.duration._

object Solvers {

  val all = List(OPTIC, FAPE)

  def apply(name: String): ISolver = all.find(_.id == name) match {
    case Some(solver) => solver
    case _            => throw new Exception(s"Unknown solver: $name")
  }
}

trait ISolver {

  def maxConcurrentRuns: Int = 1
  lazy val semaphore = scalaz.zio.Semaphore(maxConcurrentRuns)

  def id: String
  def accepts(i: DomainVariant): Boolean

  def runner(instance: ProblemId)(implicit p: Params): IO[Exception, Unit]

  def extract(instance: ProblemId)(implicit p: Params): IO[Exception, RunResult]

  def workDir(i: ProblemId)(implicit p: Params): Path =
    p.cache / id / i.domain.domain.name / i.domain.variantName / i.pbId

  def setupWorkDir(instance: ProblemId)(implicit p: Params) = IO.succeedLazy {
    os.makeDir.all(workDir(instance))
  }

  def binders(instance: ProblemId)(implicit p: Params): Props =
    instance.domain.props
      .add("log-file", "./log")
      .add("summary-file", "./summary")
      .add("plan-file", "./plan")
      .add("TIMEOUT", p.timeout.toSeconds.toString)
      .add("memory-limit", p.memoryLimit.toString)
      .add("PB", instance.pbId)
      .makeAbs(workDir(instance))

  override def toString: String = id
}

case class Error(msg: String) extends Exception(msg)

object OPTIC extends ISolver {
  val id = "optic"

  override def accepts(i: DomainVariant): Boolean = i.props("lang") == "pddl"

  val template = "runsolver -W {TIMEOUT} --vsize-limit {memory-limit} -v {summary-file} -o {log-file} " +
    "optic-clp -N {domain-file} {problem-file}"

  override def runner(instance: ProblemId)(
      implicit p: Params): IO[Exception, Unit] = {
    val props = binders(instance)
    val command = Pattern.bind(template, props.toMap)
    val wd = workDir(instance)

    for {
      _ <- scalaz.zio.console.putStrLn(s"Running $command")
      _ <- setupWorkDir(instance)
      _ <- IO.syncException {
        os.proc(command.split(" "))
          .call(
            cwd = wd,
            check = false
          )
      }
      _ <- nio
        .existsAndContains(Path(props("log-file")), "Number of literals")
        .flatMap {
          case true => IO.unit
          case false =>
            IO.fail(Error(s"problem with log file on run of $id / $instance"))
        }

    } yield ()
  }

  override def extract(instance: ProblemId)(
      implicit p: Params): IO[Exception, RunResult] = {
    val props = binders(instance)

    def readProperties(p: Path): IO[Exception, Map[String, String]] =
      IO.syncException {
        os.read.lines.stream(p).fold(Map[String, String]()) {
          case (acc, l) if l.startsWith("#") => acc
          case (acc, l) =>
            l.split("=") match {
              case Array(prop, value) => acc.updated(prop, value)
              case _                  => throw new Exception(s"Malformed line in properties: $l")
            }
        }
      }

    val costPrefix = "; Cost: "

    for {
      summary <- readProperties(Path(props("summary-file")))
      status <- if (summary("TIMEOUT") == "true")
        IO.succeed(Timeout)
      else if (summary("MEMOUT") == "true")
        IO.succeed(Memout)
      else
        nio
          .read(Path(props("log-file")))
          .map(
            content =>
              if (content.contains("Problem unsolvable")) UNSAT
              else if (content.contains("Solution Found")) SAT
              else Unknown)
      cost <- nio
        .read(Path(props("log-file")))
        .map(_.lines.foldLeft[Option[Double]](None) {
          case (prev, l) if l.startsWith(costPrefix) =>
            val newCost = l.replaceFirst(costPrefix, "").trim.toDouble
            prev match {
              case Some(c) if c < newCost => Some(c)
              case _                      => Some(newCost)
            }
          case (prev, _) => prev
        })
      time <- IO.syncException(summary("WCTIME").toDouble.seconds)
    } yield RunResult(instance, id, status, time, cost)

  }
}

object FAPE extends ISolver {
  val id = "fape"

  override def accepts(i: DomainVariant): Boolean = i.props("lang") == "anml"

  val template =
    "runsolver -v {summary-file} -o {log-file} fape -t {TIMEOUT} {problem-file} --write-plan {plan-file}"

  override def runner(instance: ProblemId)(
      implicit p: Params): IO[Exception, Unit] = {
    val props = binders(instance)
    val command = Pattern.bind(template, props.toMap)
    val wd = workDir(instance)

    for {
      _ <- scalaz.zio.console.putStrLn(s"Running $command")
      _ <- setupWorkDir(instance)
      _ <- IO.syncException {
        os.proc(command.split(" "))
          .call(
            cwd = wd,
            check = false
          )
      }
      // check if the log file match what we expect
      _ <- nio.existsAndContains(Path(props("log-file")), "strategy").flatMap {
        case true => IO.unit
        case false =>
          IO.fail(Error(s"problem with log file on run of $id / $instance"))
      }

    } yield ()
  }

  override def extract(instance: ProblemId)(
      implicit p: Params): IO[Exception, RunResult] = {
    val wd = workDir(instance)
    val props = binders(instance)

    def readProperties(p: Path): IO[Exception, Map[String, String]] =
      IO.syncException {
        os.read.lines.stream(p).fold(Map[String, String]()) {
          case (acc, l) if l.startsWith("#") => acc
          case (acc, l) =>
            l.split("=") match {
              case Array(prop, value) => acc.updated(prop, value)
              case _                  => throw new Exception(s"Malformed line in properties: $l")
            }
        }
      }

    for {
      status <- if (os.exists(Path(props("plan-file")))) IO.succeed(SAT)
      else {
        nio.existsAndContains(Path(props("log-file")), "strategy").map {
          case true  => Timeout
          case false => Unknown
        }
      }
      cost <- nio.read(Path(props("log-file")))
        .map(content => content.lines.find(_.startsWith("Makespan: ")) match {
        case Some(l) => Some(l.replaceFirst("Makespan:", "").trim.toDouble -1)
        case None => None
      })
      props <- readProperties(Path(props("summary-file")))
      time = props("WCTIME").toDouble.seconds
    } yield RunResult(instance, id, status, time, cost)

  }
}
