import scala.collection.immutable.HashMap
import scala.util.chaining._
import spire.algebra._
import spire.implicits._
import spire.math._
import spire.random._

case class ENV(
    lines: HashMap[PATH, ANY] = HashMap(),
    code: ANY.FN = ANY.FN((None, 0), List()),
    stack: Vector[ANY] = Vector(),
    scope: HashMap[ANY, ANY] = HashMap(),
    arr: List[Vector[ANY]] = List(),
    // rng: Uniform[Long],
    eS: Boolean = false,
    eV: Boolean = false,
    eI: Boolean = false
)

object ENV:

  def exec(env: ENV) = env.code match
    case ANY.FN(_, List()) => env
    case ANY.FN(p, c :: cs) =>
      if env.eS || env.eI then println(ENV.toString)
      env.copy(code = ANY.FN(p, cs))

  def run(o: (Boolean, Boolean, Boolean), f: FILE, l: String) =
    ENV(l.lines.zipWithIndex.map { case (x, i) => ((f, i), ANY.STR(x)) }.pipe(HashMap.from))
