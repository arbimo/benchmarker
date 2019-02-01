package bench

import scalaz.zio._
import scalaz.zio.console._
import scalaz.zio.interop.catz._

import caseapp._
import caseapp.core.app.CaseApp

import cats.implicits._
import os.Path

import scala.concurrent.duration._

import bench.implicits._

case class Params(
    @ExtraName("d")
    domains: List[String] = List(),
    planners: List[String] = List("optic", "fape"),
    timeout: Duration = 40.seconds,
    domainsDirectory: Path = os.pwd / "domains",
    baseDir: Path = os.pwd,
    parallelExecutions: Int = 3,
    memoryLimit: Int = 2000,
    dryRun: Boolean = true,
    force: Boolean = false,
    failFast: Boolean = false
) {
  def cache = baseDir / "out"
}

object Pattern {

  def bind(
      str: String,
      map: Map[String, String],
      partial: Boolean = true
  ): String = {
    val split = str.split("[{}]")
    val sb = new StringBuilder()
    for (i <- split.indices) {
      if (i % 2 == 0) {
        // not a pattern
        sb.append(split(i))
      } else {
        val key = split(i)
        if (map.contains(key)) {
          sb.append(map(key))
        } else if (partial) {
          sb.append(s"{$key}")
        } else {
          sys.error(s"Key '$key' is missing from binds: $map")
        }

      }
    }
    sb.toString()
  }

}

object Main extends scalaz.zio.App {

  def instances(dv: DomainVariant) = {
    FileMatcher.findFilesMatching(
      dv.domain.directory,
      dv.props("problem-file"),
      "PB"
    ).filterNot(_.endsWith("_ck")) // for tpp instances
  }

  override def run(args: List[String]): IO[Nothing, ExitStatus] = {
    implicit val params: bench.Params =
      CaseApp.parse[Params](args) match {
        case Right((p, _)) => p
        case Left(err) =>
          return for {
            _ <- putStrLn(err.message)
            _ <- putStrLn(CaseApp.helpMessage[Params])
          } yield ExitStatus.ExitNow(1)
      }

    // val xx = os.walk(params.domainsDirectory).filter(_.last == "domain.toml")
    val domsIO = for {
      files <- nio.walk(params.domainsDirectory, skip = _.last == ".git")
      confFiles = files.filter(_.last == "domain.toml").toList
      _ <- putStrLn(confFiles.mkString("\n"))
      allDomains <- IO.collectAllPar(confFiles.map(Conf.parse))
      _ <- putStrLn(allDomains.toString)
      domains = if (params.domains.isEmpty)
        allDomains.flatten
      else
        allDomains.flatten.filter(v => params.domains.contains(v.domain.name))
    } yield domains

    val solvers = params.planners.map(Solvers(_))

    val runsIO: IO[String, List[Run]] = domsIO.map(domains => {
      for {
        solver <- solvers
        dom <- domains
        if solver.accepts(dom)
        pb <- instances(dom)
        id = ProblemId(dom, pb)
      } yield Run(solver, id)
    })

    val program: IO[String, ExitStatus] = for {
      sem <- Semaphore(params.parallelExecutions)
      runs <- runsIO
      evals = runs
        .map(_.eval(useCache = !params.force, sem = sem).leftMap(_.toString))
      res <- IO.collectAllPar(evals)
      _ <- Stats.print(res)
    } yield {
      ExitStatus.ExitNow(0)
    }
    program.redeem(msg =>
                     putStrLn(s"error: $msg")
                       .map(_ => ExitStatus.ExitNow(1)),
                   x => IO.succeed(x))

  }
}
