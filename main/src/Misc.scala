import java.math.MathContext
import org.apfloat.{ApfloatMath => Ap, FixedPrecisionApfloatHelper => Afp, _}
import scala.language.implicitConversions
import util.chaining._

type FILE = Option[os.Path]

type ARRW = Vector[ANY]
type SEQW = LazyList[ANY]
type MAPW = Map[ANY, ANY]

type NUMF = Apfloat

object NUMF:

  implicit def IntAp(n: Int): NUMF       = Apfloat(n)
  implicit def StringAp(s: String): NUMF = Apfloat(s)

type LINESW = Map[PATH, ANY]

case class PATH(f: FILE, l: Int):

  override def toString: String = s"${f.getOrElse("?")}:$l"

case class LinERR(p: PATH, t: String, x: String) extends Exception(x):

  override def toString: String = s"ERR($t): $x @ ($p)"

  override def equals(x: Any): Boolean = x match
    case x: LinERR => canEqual(x) && t == x.t

  override def hashCode: Int = t.##
