import cats.Functor
import os.{Path, RelPath}
import toml.Value.{Str, Tbl}
import toml.{Codec, Value}
//
//sealed trait Val {
//  def map(f: String => String): Val
//  def makeAbs(dir: Path): Val = this
//}
//case class Str(str: String) extends Val {
//  require(!str.startsWith("/") && !str.startsWith("./"))
//
//  override def map(f: String => String): Val = Str(f(str))
//}
//case class RelFile(str: String) extends Val {
//  require(str.startsWith("./"))
//  override def makeAbs(dir: Path): AbsFile = AbsFile((dir / RelPath(str)).toString)
//
//  override def map(f: String => String): Val = RelFile(f(str))
//}
//case class AbsFile(str: String) extends Val {
//  require(str.startsWith("/"))
//  override def map(f: String => String): Val = AbsFile(f(str))
//}
//case class Lst(l: List[Val]) extends Val {
//  override def map(f: String => String): Val = Lst(l.map(_.map(f)))
//  override def makeAbs(dir: Path): Val = Lst(l.map(_.makeAbs(dir)))
//}
//
//case class Props(kv: Map[String, Val]) extends Val {
//
//  private def asPF: PartialFunction[String, String] = new PartialFunction[String,String] {
//    override def isDefinedAt(x: String): Boolean = kv.get(x) match {
//      case Some(Str(s)) => true
//      case _ => false
//    }
//
//    override def apply(x: String): String = kv.get(x) match {
//      case Some(Str(s)) => s
//      case _ => ???
//    }
//  }
//
//  def add(key: String, value: Val): Props = {
//    val v2 = value.map(Props.bind(_, this.asPF))
//    val m2 = kv.mapValues(v => v.map(Props.bind(_, Props(Map(key -> v2)).asPF)))
//    Props(m2.updated(key, v2))
//  }
//
//  def makeAbsolute(dir: Path): Props = {
//    Props(      kv.mapValues(_.makeAbs(dir))    )
//  }
//
//  override def map(f: String => String): Val = Props(kv.mapValues(_.map(f)))
//}
//object Props {
//  def bind(str: String, map: PartialFunction[String, String], partial: Boolean = true): String = {
//    val split = str.split("[{}]")
//    val sb = new StringBuilder()
//    for(i <- split.indices) {
//      if(i %2 == 0) {
//        // not a pattern
//        sb.append(split(i))
//      } else {
//        val key = split(i)
//        if(map.isDefinedAt(key)) {
//          sb.append(map(key))
//        } else if(partial) {
//          sb.append(s"{$key}")
//        } else {
//          sys.error(s"Key '$key' is missing from binds: $map")
//        }
//
//
//      }
//    }
//    sb.toString()
//  }
//}

case class Props(m: Tbl) {
  def apply(k: String) = Conf.read(m, k)
  def add(k: String, v: String) = Props(Conf.add(m)(k, v).asInstanceOf[Tbl])

  def toMap: Map[String,String] = m.values.mapValues(Props.str(_))
}
object Props {
  def empty: Props = Props(Tbl(Map()))
  def fromMap(m: Map[String,String]): Props = Props(Tbl(m.mapValues(Str(_))))

  def str(v: toml.Value): String = v match {
    case Str(s) => s
    case _ => ???
  }

  implicit val enc: upickle.default.ReadWriter[Props] = upickle.default.readwriter[Map[String,String]].bimap[Props](
    v => v.toMap    ,
  m => Props.fromMap(m)
  )
}


object Conf extends App {


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

  def parse(f: Path): Seq[DomainVariant] = {
    val str = os.read(f)
    val dir = f / os.up
    toml.Toml.parse(str) match {
      case Left(err) =>
        sys.error(s"Failed to parse $f: $err")
      case Right(v) =>
        val v2 = makeAbs(v)(dir)
        val v3 = bindAll(v2)
        flatten(v3.asInstanceOf[Tbl]).map {
          case (k, v: Tbl) =>
            val dom = Domain(read(v, "DOM"), dir, Props.empty)
            DomainVariant(dom, variantName = k, Props(v))
        }.toSeq
    }
  }

  val x = toml.Toml.parse(conf).getOrElse(???)

  println(x)
  val a = makeAbs(x)(Path("/root/"))
  val b = bindAll(a)
  val c = flatten(b.asInstanceOf[Tbl])

  println(a)
  println(b)

  c.foreach(println)


}
