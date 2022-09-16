import scala.util.chaining._
import spire.algebra._
import spire.implicits._
import spire.math._
import spire.random._

case class ENV(
    lines: Map[PATH, ANY] = Map(),
    code: ANY.FN = ANY.FN((None, 0), List()),
    stack: Vector[ANY] = Vector(),
    scope: Map[ANY, ANY] = Map(),
    arr: List[Vector[ANY]] = List(),
    // rng: Uniform[Long],
    eS: Boolean = false,
    eV: Boolean = false,
    eI: Boolean = false
)

object ENV:

  implicit class ENV1(env: ENV):

    def pprint = println(env.toString)

    def modCode(f: List[ANY] => List[ANY]) =
      env.copy(code = ANY.FN(env.code.p, f(env.code.x)))

    def exec = env.code match
      case ANY.FN(_, List()) => env
      case ANY.FN(p, c :: cs) =>
        if env.eS || env.eI then env.pprint
        env.modCode(_ => cs)

  def run(o: (Boolean, Boolean, Boolean), f: FILE, l: String) =
    val l1 = l.lines.zipWithIndex.map { case (x, i) => ((f, i), ANY.STR(x)) }
      .pipe(Map.from)
    ENV(l1).exec.tap(env => if env.eS || env.eV || env.eI then env.pprint)
