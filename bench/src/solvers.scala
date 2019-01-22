
import SolveStatus.{Memout, SAT, Timeout, UNSAT, Unknown}
import cats._
import cats.implicits._
import cats.effect._
import os.{Path, RelPath}

import scala.concurrent.duration._


object Solvers {
  def apply(name: String): ISolver = name match {
    case "optic" => OPTIC
    case _ => throw new Exception(s"Unknown solver: $name")
  }
}


trait ISolver {
  def id: String
  def accepts(i: DomainVariant): Boolean

  def runner(instance: ProblemId)(implicit p: Params): IO[RunLog]

  def extract(instance: ProblemId, log: RunLog)(implicit p: Params): IO[RunResult]

  def workDir(i: ProblemId)(implicit p: Params): Path =
    p.cache / id / i.domain.domain.name / i.domain.variantName / i.pbId

  def setupWorkDir(instance: ProblemId)(implicit p: Params) = IO {
    val wd = instance.domain.domain.directory
    os.makeDir.all(wd)
//    val files = instance.props
//      .values
//      .map{ RelPath(_) }
//      .filter { f =>  os.exists(wd / f) && os.isFile(wd / f)  }
//    for(f <- files)
//      os.copy(wd / f, workDir(instance) / f, replaceExisting = true, copyAttributes = true, createFolders = true)
  }

  def binders(instance: ProblemId)(implicit p: Params) =
    instance.domain.props
      .add("log-file", "log")
        .add("summary-file", "summary")
        .add("TIMEOUT", p.timeout.toSeconds.toString)
        .add("memory-limit", p.memoryLimit.toString)
        .add("PB", instance.pbId)
//    Map[String,String](
//    "log-file" -> "log",
//    "summary-file" -> "summary",
//    "TIMEOUT" -> p.timeout.toSeconds.toString,
//    "memory-limit" -> p.memoryLimit.toString,
//    "PB" -> instance.pbId
//  ) ++ instance.domain.props.mapValues(Pattern.bind(_, Map("PB" -> instance.pbId)))


  override def toString: String = id
}

object OPTIC extends ISolver {
  val id = "optic"

  override def accepts(i: DomainVariant): Boolean = i.props("lang") == "pddl"

  val template = "runsolver -W {TIMEOUT} --vsize-limit {memory-limit} -v {summary-file} -o {log-file} " +
    "/home/arthur/work/ext/optic/release/optic/optic-clp -N {domain-file} {problem-file}"

  override def runner(instance: ProblemId)(implicit p: Params): IO[RunLog] = {
    val props = binders(instance)
    val command = Pattern.bind(template, props.toMap)
    val wd = workDir(instance)

    val run = IO {
      println(s"Running $command")
      os.makeDir.all(wd)
      os
        .proc(command.split(" "))
        .call(
          cwd = wd,
          check = false
        )
    }
    val logs = IO.pure(RunLog(wd / props("summary-file"), wd / props("log-file")))
    setupWorkDir(instance) *>
      run *> logs
  }

  override def extract(instance: ProblemId, log: RunLog)(implicit p: Params): IO[RunResult] = {


    def readProperties(p: Path): IO[Map[String,String]] = IO {
      os.read.lines.stream(log.summary).fold(Map[String,String]()){
        case (acc, l) if l.startsWith("#") => acc
        case (acc, l) =>
          l.split("=") match {
            case Array(prop, value) => acc.updated(prop, value)
            case _ => throw new Exception(s"Malformed line in properties: $l")
          }
      }
    }

    for {
      props <- readProperties(log.summary)
      status <- IO {
        if(props("TIMEOUT") == "true")
          Timeout
        else if(props("MEMOUT") == "true")
          Memout
        else
          os.read.lines.stream(log.output).fold[SolveStatus](Unknown) {
            case (Unknown, l) if l.contains("Problem unsolvable") => UNSAT
            case (Unknown, l) if l.contains("Solution Found") => SAT
            case (prev, _) => prev
          }
      }
      time = props("WCTIME").toDouble.seconds
    } yield RunResult(instance, id, status, time)



  }
}


object FAPE extends ISolver {
  val id = "fape"

  override def accepts(i: DomainVariant): Boolean = i.props("lang") == "anml"

//  val template = "runsolver -W {TIMEOUT} --vsize-limit {memory-limit} -v {summary-file} -o {log-file} " +
//    "/home/arthur/work/ext/optic/release/optic/optic-clp -N {domain-file} {problem-file}"
  val template = "ng-nailgun fr.laas.fape.planning.Planning"

  override def runner(instance: ProblemId)(implicit p: Params): IO[RunLog] = {
    val props = binders(instance)
    val command = Pattern.bind(template, props.toMap)
    val wd = workDir(instance)

    val run = IO {
      println(s"Running $command")
      os
        .proc(command.split(" "))
        .call(
          cwd = wd,
          check = false
        )
    }
    val logs = IO.pure(RunLog(wd / props("summary-file"), wd / props("log-file")))
    setupWorkDir(instance) *>
      run *> logs
  }

  override def extract(instance: ProblemId, log: RunLog)(implicit p: Params): IO[RunResult] = {


    def readProperties(p: Path): IO[Map[String,String]] = IO {
      os.read.lines.stream(log.summary).fold(Map[String,String]()){
        case (acc, l) if l.startsWith("#") => acc
        case (acc, l) =>
          l.split("=") match {
            case Array(prop, value) => acc.updated(prop, value)
            case _ => throw new Exception(s"Malformed line in properties: $l")
          }
      }
    }

    for {
      props <- readProperties(log.summary)
      status <- IO {
        if(props("TIMEOUT") == "true")
          Timeout
        else if(props("MEMOUT") == "true")
          Memout
        else
          os.read.lines.stream(log.output).fold[SolveStatus](Unknown) {
            case (Unknown, l) if l.contains("Problem unsolvable") => UNSAT
            case (Unknown, l) if l.contains("Solution Found") => SAT
            case (prev, _) => prev
          }
      }
      time = props("WCTIME").toDouble.seconds
    } yield RunResult(instance, id, status, time)



  }
}