import os.Path
import upickle.default._

import scala.util.matching._
import Pickler._
import caseapp.ExtraName
import caseapp.core.app.CaseApp
import cats._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Semaphore

import scala.concurrent.duration._
import implicits._

case class Params(
                 @ExtraName("d")
                   domains: List[String] = List("rovers_ipc5", "blocks_ipc2"),
                   planners: List[String] = List("optic"),
                   timeout: Duration = 40.seconds,
                   domainsDirectory: Path = os.pwd / "domains",
                   baseDir: Path = os.pwd / "cache",
                   parallelExecutions: Int = 3,
                   memoryLimit: Int = 2000
                 ) {
  def cache = baseDir / "cache"
}


object Pattern {

  def bind(str: String, map: Map[String, String], partial: Boolean = true): String = {
    val split = str.split("[{}]")
    val sb = new StringBuilder()
    for(i <- split.indices) {
      if(i %2 == 0) {
        // not a pattern
        sb.append(split(i))
      } else {
        val key = split(i)
        if(map.contains(key)) {
          sb.append(map(key))
        } else if(partial) {
          sb.append(s"{$key}")
        } else {
          sys.error(s"Key '$key' is missing from binds: $map")
        }


      }
    }
    sb.toString()
  }

}


object Main extends IOApp {



  def instances(dv: DomainVariant) = {
    FileMatcher.findFilesMatching(dv.domain.directory, dv.props("problem-file"), "PB")
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val params =
      CaseApp.parse[Params](args)  match {
        case Right((p, _)) => p
        case Left(err) =>
          return IO.delay(println(err.message)) *>
            IO.delay(println(CaseApp.helpMessage[Params])) *>
            IO.pure(ExitCode.Error)
      }

    val xx = os.walk(params.domainsDirectory).filter(_.last == "domain.toml")
    println(xx)
    val domains = xx.toList.flatMap { p =>
      val str = os.read(p)
      val dir = p / os.up
      Conf.parse(p)
    }
    domains.foreach(d => {
      val x = instances(d)
      println(d)
      println(s"instances: ${x.mkString(", ")}")
    })

    val solvers = params.planners.map(Solvers(_))
    val runs =
      for {
        solver <- solvers
        dom <- domains
        if solver.accepts(dom)
        pb <- instances(dom)
      } yield {
        val id = ProblemId(dom, pb)
        Run(solver, id)
      }

    for {
      sem <- Semaphore.apply[IO](params.parallelExecutions)
      res <- runs.toList.map(_.eval(sem = sem)).parSequence

    } yield {
      Stats.print(res)

      ExitCode.Success
    }
  }
}
