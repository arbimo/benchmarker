package bench

import java.io.IOException

import scalaz.zio._
import os.{Path, RelPath}
import toml.Value
import toml.Value.{Arr, Str, Tbl}

import scala.util.Try

case class Props(m: Tbl) {
  def apply(k: String) = Conf.read(m, k)
  def add(k: String, v: String) = Props(Conf.add(m)(k, v).asInstanceOf[Tbl])
  def makeAbs(dir: os.Path): Props = Props(Conf.makeAbs(m)(dir).asInstanceOf[Tbl])

  def toMap: Map[String,String] = m.values.mapValues(Props.str(_))
}
object Props {
  def empty: Props = Props(Tbl(Map()))
  def fromMap(m: Map[String,String]): Props = Props(Tbl(m.mapValues(Str(_))))

  def str(v: toml.Value): String = v match {
    case Str(s) => s
    case Arr(l) => l.map(str(_)).mkString("{", ":", "}")
    case _ => ???
  }

  implicit val enc: upickle.default.ReadWriter[Props] = upickle.default.readwriter[Map[String,String]].bimap[Props](
    v => v.toMap    ,
  m => Props.fromMap(m)
  )
}


object Conf  {


    val conf = """
DOM="blocks_ipc2"
[pddl]
lang="pddl"
domain-file="./pddl/{DOM}.dom.pddl"
problem-file="./pddl/{DOM}.{PB}.pb.pddl"
[anml-flat]
lang="anml"
domain-file="./anml/{DOM}.dom.anml"
problem-file="./anml/{DOM}.{PB}.pb.anml"
[anml-part-hier]
lang="anml"
features = ["htn"]
domain-file="./anml-part-hier/{DOM}-hier.dom.anml"
problem-file="./anml-part-hier/{DOM}-hier.{PB}.pb.anml"
    """


  import toml.Value._

  def trans(v: toml.Value)(f: String => String): toml.Value = v match {
    case Str(s) => Str(f(s))
    case Tbl(kv) => Tbl(kv.mapValues(trans(_)(f)))
    case Arr(l) => Arr(l.map(trans(_)(f)))
    case x => x
  }
  def makeAbs(v: toml.Value)(dir: Path): toml.Value = {
    val f: String => String = s => {
      if(s.startsWith("./"))
        (dir / RelPath(s)).toString()
      else
        s
    }
    trans(v)(f)
  }
  def bindings(v: toml.Value): Map[String,String] = v match {
    case Tbl(kv) => kv.collect {
      case (k, Str(v)) => (k, v)
    }.toMap
    case _ => Map()
  }
  def bindAll(v: toml.Value, base: Map[String,String] = Map()): toml.Value = v match {
    case Tbl(kv) =>
      val bs = bindings(v) ++ base
      Tbl(kv.mapValues(bindAll(_, bs)))
    case Str(s) =>
      Str(Pattern.bind(s, base))
    case Arr(l) => Arr(l.map(bindAll(_, base)))
    case x => x
  }
  def flatten(tbl: Tbl): Map[String, Value] = {
    val prims = tbl.values.filter(!_._2.isInstanceOf[Tbl])
    val nested = tbl.values.collect {
      case (k, kv: Tbl) => (k, Tbl(prims ++ kv.values ))
    }.toMap
    nested
  }
  def read(v: toml.Value, key: String): String = v match {
    case Tbl(m) => m.get(key) match {
      case Some(Str(v)) => v
      case _ => ???
    }
    case _ => ???
  }
  def add(x: toml.Value)(k: String, v: String) = x match {
    case Tbl(m) => bindAll(Tbl(m.updated(k, Str(v))))
    case _ => ???
  }

  def parse(f: Path): IO[String, List[DomainVariant]] = {
    val dir = f / os.up

    nio.read(f)
      .leftMap(ioError => ioError.toString)
      .map(str => toml.Toml.parse(str))
      .flatMap {
        case Left(err) => IO.fail(s"Failed to parse $f: $err")
        case Right(v) => IO.succeed(v)
      }.map(v => {
        val v2 = makeAbs(v)(dir)
        val v3 = bindAll(v2)
        flatten(v3.asInstanceOf[Tbl]).map {
          case (k, v: Tbl) =>
            val dom = Domain(read(v, "DOM"), dir, Props.empty)
            DomainVariant(dom, variantName = k, Props(v))
        }.toList
    })
  }



}
