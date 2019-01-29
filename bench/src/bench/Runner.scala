package bench

import SolveStatus.{Memout, SAT, Timeout, UNSAT, Unknown}
import Stats.SolverConf
import cats._
import cats.implicits._
import scalaz.zio._
import scalaz.zio.interop.catz._
import os.Path

import scala.concurrent.duration._
import upickle.default._
import upickle._

object Pickler {
  implicit val pathPickler: ReadWriter[Path] =
    readwriter[String].bimap[Path](_.toString(), Path(_))
}
import Pickler._

case class RunLog(workDir: os.Path)

sealed trait SolveStatus {
  def solved = this match {
    case SolveStatus.Unknown => false
    case SolveStatus.SAT     => true
    case SolveStatus.UNSAT   => true
    case SolveStatus.Timeout => false
    case SolveStatus.Memout  => false
  }
}
object SolveStatus {
  case object Unknown extends SolveStatus
  case object SAT extends SolveStatus
  case object UNSAT extends SolveStatus
  case object Timeout extends SolveStatus
  case object Memout extends SolveStatus

  implicit val rw: ReadWriter[SolveStatus] = macroRW
}

case class Instance(domain: String, problem: String) {
  override def toString: String = s"$domain/$problem"
}

case class RunResult(pb: ProblemId,
                     solver: String,
                     status: SolveStatus,
                     time: Duration,
                     cost: Option[Double]) {
  override def toString: String =
    s"$solver\t${pb.domain}\t${pb.pbId}\t$status\t$time"

  def solverConf: SolverConf = SolverConf(solver, pb.domain.variantName)
  def domain: String = pb.domain.domain.name
  def instance: Instance = Instance(domain, pb.pbId)
}
object RunResult {
  implicit val rw: ReadWriter[RunResult] = macroRW
}

case class Run(solver: ISolver, problemId: ProblemId) {

  def resultDir(implicit p: Params): Path =
    p.cache / solver.id / problemId.domain.domain.name / problemId.domain.variantName / problemId.pbId

  def retrieveCached(implicit p: Params): IO[Nothing, Option[RunResult]] =
    solver
      .extract(problemId)
      .map {
        case RunResult(_, _, Unknown, _, _)                  => None
        case RunResult(_, _, Timeout, t, _) if t < p.timeout => None
        case x if x.time > p.timeout =>
          Some(x.copy(status = Timeout, time = p.timeout))
        case x => Some(x)
      }
      .redeemPure(_ => None, x => x)

  def eval(useCache: Boolean = true, sem: Semaphore)(implicit p: Params): IO[Exception, RunResult] = {

    for {
      base <- if (useCache) retrieveCached
      else IO.succeed(None)
      res <- base match {
        case Some(x) => IO.succeed(x)
        case None =>
          for {
            _ <- sem.acquire
            run = if (p.dryRun) IO.unit
            else solver.runner(problemId)
            _ <-
              if(p.failFast) run
              else run.orElse(IO.unit)

            _ <- sem.release
            res <- solver
              .extract(problemId)
              .redeemPure(
                _ =>
                  RunResult(problemId, solver.id, Unknown, Duration.Inf, None),
                identity)
          } yield res
      }
    } yield res

  }

}

