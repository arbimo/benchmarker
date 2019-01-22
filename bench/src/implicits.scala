
import caseapp.core.argparser.{ArgParser, SimpleArgParser}
import os.Path
import upickle.default._

import scala.concurrent.duration._
import scala.util.{Success, Try}

object implicits {

  implicit val pathPickler : ReadWriter[Path] = readwriter[String].bimap[Path](_.toString(), Path(_))

  implicit val pathParser: ArgParser[Path] = SimpleArgParser.from[Path]("path") { s =>
    Right(Path(s))
  }

  implicit val durationParser : ArgParser[Duration] = SimpleArgParser.from[Duration]("duration") { s =>
    Try(s.toInt) match {
      case Success(i) => Right(i.seconds)
      case _ => Left(caseapp.core.Error.MalformedValue("duration", s"Expected a rond number of seconds but got '$s'"))
    }
  }

}
