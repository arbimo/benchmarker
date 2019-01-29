package bench



import caseapp._
import caseapp.core.app.CaseApp
import cats.effect._
import cats.effect.concurrent.Semaphore
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
                   baseDir: Path = os.pwd ,
                   parallelExecutions: Int = 3,
                   memoryLimit: Int = 2000,
                 dryRun: Boolean = true,
                 force: Boolean = false
                 ) {
  def cache = baseDir / "out"
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
    implicit val params: bench.Params =
      CaseApp.parse[Params](args)  match {
        case Right((p, _)) => p
        case Left(err) =>
          return IO.delay(println(err.message)) *>
            IO.delay(println(CaseApp.helpMessage[Params])) *>
            IO.pure(ExitCode.Error)
      }

    val xx = os.walk(params.domainsDirectory).filter(_.last == "domain.toml")
    println(xx)
    val allDomains = xx.toList.flatMap { p =>
      val str = os.read(p)
      val dir = p / os.up
      Conf.parse(p)
    }
    val domains =
      if(params.domains.isEmpty) allDomains
      else allDomains.filter(v => params.domains.contains(v.domain.name))

//    targetDomains.foreach(d => {
//      val x = instances(d)
//      println(d)
//      println(s"instances: ${x.mkString(", ")}")
//    })

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
      res <- runs.toList.map(_.eval(useCache = !params.force, sem = sem)).parSequence

    } yield {
      Stats.print(res)

      ExitCode.Success
    }
  }
}
