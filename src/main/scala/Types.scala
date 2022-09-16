import scala.collection.immutable.HashMap
import spire.algebra._
import spire.implicits._
import spire.math._

type FILE = Option[os.Path]
type PATH = (FILE, Int)

enum ANY:

  case ARR(x: Vector[ANY])
  case MAP(x: HashMap[ANY, ANY])
  case SEQ(x: LazyList[ANY])
  case NUM(x: Rational)
  case STR(x: String)
  case CMD(x: String)
  case FN(p: PATH, x: List[ANY])
  case UN
