package bench


import bench.Properties.MSS
import os.Path
import upickle.default._
import bench.implicits._

case class Properties(top: MSS, subs: Map[String, MSS]) {
  override def toString: String = {
    Properties.format(top, 0) + "\n" + subs.map{ case (k, v) => s"[$k]\n" + Properties.format(v, 2)}.mkString("\n")
  }

}
object Properties {
  type MSS = Map[String, String]

  private def format(m: MSS, ident: Int) = m.map(a => " "*ident + s"${a._1} <- ${a._2}").mkString("\n")


  val keyValuePattern = "^\\s*([^=\\s]+)\\s*=\\s*([^\\s]+)$".r
  val sectionPattern = "^\\s*\\[([^\\[]+)\\]\\s*$".r

  def parse(str: String): Properties = {
    def parseUntilSub(lines: List[String], kvs: MSS = Map()): (List[String], MSS) = lines match {
      case keyValuePattern(k, v) :: tail => parseUntilSub(tail, kvs.updated(k, v))
      case _ => (lines, kvs)
    }
    def parseAllSub(lines: List[String], previous: Map[String, MSS] = Map()): Map[String, MSS] = lines match {
      case Nil => previous
      case sectionPattern(secName) :: tail =>
        val (rest, cur) = parseUntilSub(tail)
        parseAllSub(rest, previous.updated(secName, cur))
      case  l :: _ =>
        sys.error(s"Cannot parse: $l")
    }
    val (rest, top) = parseUntilSub(str.lines.toList, Map())
    val subs = parseAllSub(rest)
    Properties(top, subs)
  }
}

case class Domain(name: String, directory: Path, props: Props) {
  override def toString: String = name
}
object Domain {
  implicit val rw: ReadWriter[Domain] = macroRW
}
case class DomainVariant(domain: Domain, variantName: String, props: Props) {
  override def toString: String = s"$domain/$variantName"
}
object DomainVariant {
  implicit val rw: ReadWriter[DomainVariant] = macroRW

}

case class ProblemId(domain: DomainVariant, pbId: String) {
  override def toString: String = s"$domain:$pbId"
  val props = domain.props.add("PB", pbId)
  //domain.props.mapValues(Pattern.bind(_, Map("PB" -> pbId)))
}
object ProblemId {
  implicit val rw: ReadWriter[ProblemId] = macroRW
}
//case class Instance(problemId: ProblemId, properties: Map[String, String] = Map()) {
//  def kind: String = ???
//  def wd: Path = ???
//}
//object Instance {
//  implicit val rw: ReadWriter[Instance] = macroRW
//}


object FileMatcher {

  def findFilesMatching(dir: Path, pattern: String, varPart: String): Seq[String] = {
    val p = pattern
      .replaceAllLiterally(".", "\\.")
      .replace("{PB}", "([^\\.]+)")
      .r

    if(os.exists(dir))
      os.walk(dir)
      .map(_.toString())
        .collect  {
          case p(id) => id
        }.sorted
    else
      Seq.empty
  }
}

case class Explorer(
                     kind: String,
                     subFolder: Option[String],
                     files: Map[String, List[String]],
                     patterns: Map[String, String] = Map()
                   ) {
  require(files.contains("problem-file"))

  val problemPattern = files.get("problem-file") match {
    case Some(f :: Nil) => f
    case _ => ???
  }

  def problemIds(dom: Domain): Seq[String] = {
    val p = problemPattern
      .replace("{DOM}", dom.name)
      .replaceAllLiterally(".", "\\.")
      .replace("{PB}", "([^\\.]+)")


    val path = subFolder.map(dom.directory / _).getOrElse(dom.directory)

    val matcher = p.r
    if(os.exists(path))
      os.list(path)
        .map(_.relativeTo(dom.directory).toString())
//        .map(_.last)
        .collect  {
          case matcher(id) => id
        }
    else
      Seq.empty
  }



  def instances(dom: Domain): Seq[Instance] = {
    val path = subFolder.map(dom.directory / _).getOrElse(dom.directory)

    def getFirstExistingFile(f: List[String]): String = f match {
      case h :: _ if os.exists(path / h) => h
      case _ :: tail => getFirstExistingFile(tail)
      case Nil => ???
    }
    val wd = subFolder.map(dom.directory / _).getOrElse(dom.directory)
    for(pb <- problemIds(dom)) yield {
      val binds = Map(
        "DOM" -> dom.name,
        "PB" -> pb
      )

      val kv =
        patterns.mapValues(x => Pattern.bind(x, binds)) ++
      files
        .mapValues(fs => getFirstExistingFile(fs.map(Pattern.bind(_, binds))))

//      Instance(ProblemId(dom, pb), kind, wd , kv)
      ???
    }
  }
}