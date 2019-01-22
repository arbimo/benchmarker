
import SolveStatus.{Memout, SAT, Timeout, UNSAT, Unknown}
import cats._
import cats.implicits._
import cats.effect.IO
import cats.effect.concurrent.Semaphore
import os.Path

import scala.concurrent.duration._
import upickle.default._
import upickle._

object Pickler {
  implicit val pathPickler : ReadWriter[Path] = readwriter[String].bimap[Path](_.toString(), Path(_))
}
import Pickler._


case class RunLog(summary: Path, output: Path)





sealed trait SolveStatus {
  def solved = this match {
    case SolveStatus.Unknown => ???
    case SolveStatus.SAT => true
    case SolveStatus.UNSAT => true
    case SolveStatus.Timeout => false
    case SolveStatus.Memout => false
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




case class RunResult(pb: ProblemId, solver: String, status: SolveStatus, time: Duration) {
  override def toString: String = s"$solver\t${pb.domain}\t${pb.pbId}\t$status\t$time"
}
object RunResult {
   implicit val rw: ReadWriter[RunResult] = macroRW
}

case class Run(solver: ISolver, problemId: ProblemId) {

  def resultDir(implicit p: Params): Path =
    p.cache / solver.id / problemId.domain.domain.name / problemId.domain.variantName / problemId.pbId


  def persistentFile(implicit p: Params): Path = {
    resultDir / "run"
  }

  def cacheResult(res: RunResult)(implicit p: Params): IO[Unit] = IO {
    val json = write(res)
    os.write.over(persistentFile, json, createFolders = true)
  }

  def retrieveCached(implicit p: Params): IO[Option[RunResult]] = IO {
    val target = persistentFile
    if(os.exists(target)) {
      read[RunResult](os.read(target)) match {
        case RunResult(_, _, Unknown, _) => None
        case RunResult(_, _, Timeout, t) if t < p.timeout => None
        case x if x.time > p.timeout =>
          Some(x.copy(status = Timeout, time = p.timeout))
        case x => Some(x)
      }
    } else {
      None
    }
  }

  def eval(useCache: Boolean = true, persist: Boolean = true, sem: Semaphore[IO])(implicit p: Params): IO[RunResult] = {

//    val base =
//      if(useCache) retrieveCached
//      else IO.pure(None)

    for {
      base <-
        if(useCache) retrieveCached
        else IO.pure(None)
      res <- base match {
        case Some(x) => IO.pure(x)
        case None =>
          for {
            _ <- sem.acquire
            log <- solver.runner(problemId)
            _ <- sem.release
            res <- solver.extract(problemId, log)
            _ <- if(persist) cacheResult(res) else IO.unit
          } yield res
//          solver.runner(instance)
//            .flatMap(log => solver.extract(instance, log))
//            .map { res =>
//              if (persist)
//                cacheResult(res)
//              res
//            }
      }
    } yield res

//    base.flatMap {
//      case Some(x) => IO.pure(x)
//      case None =>
//        val c =
//          Pattern.bind(solver.runTemplate, instance.properties)
//        val run = IO {
//          val tstart = System.currentTimeMillis()
//          val res = os
//            .proc(c.split(" "))
//            .call(
//              cwd = instance.wd,
//              check = false
//            )
//          val tend = System.currentTimeMillis()
//
//          res.exitCode match {
//            case 0 =>
//              RunResult(instance, solver, status = true, time = (tend - tstart).milliseconds)
//            case err =>
//              RunResult(instance, solver, status = false, time = (tend - tstart).milliseconds)
//          }
//        }
//        run.flatMap(res => cacheResult(res).map(_ => res))
//    }
  }

}

object Runner {

//  def run(solver: Solver, instance: Instance): RunResult = {
////    println(s"Running ${solver.id} / ${instance.problemId}")
//    val c =
//      Pattern.bind(solver.runTemplate, instance.properties)
////    val command =
////      c.split(" ").map(x => StringShellable(x))
////    println(c)
//
//    val tstart = System.currentTimeMillis()
//    val res = os
//      .proc(c.split(" "))
//      .call(
//        cwd = instance.wd,
//        check = false
//      )
//    val tend = System.currentTimeMillis()
//
//    res.exitCode match {
//      case 0 =>
//        RunResult(instance, solver, success = true, time = (tend -tstart).milliseconds)
//      case err =>
//        RunResult(instance, solver, success = false, time = (tend -tstart).milliseconds)
//    }
//  }

}
