import scala.util.chaining._
import spire.algebra._
import spire.implicits._
import spire.math._
import spire.random._

case class ENV(
    lines: Map[PATH, ANY] = Map(),
    code: ANY.FN,
    stack: Vector[ANY] = Vector(),
    scope: Map[ANY, ANY] = Map(),
    arr: List[Vector[ANY]] = List(),
    // rng: Uniform[Long],
    eS: Boolean = false,
    eV: Boolean = false,
    eI: Boolean = false
):

  def trace1 =
    println(fansi.Color.DarkGray("———>"))
    println(code.x match
      case List() => fansi.Color.Green("(EMPTY)")
      case c :: cs =>
        fansi.Str.join(
          Seq(
            fansi.Bold.On(fansi.Color.Yellow(c.toString)),
            fansi.Color.DarkGray(
              if cs.length > 5 then s"${cs.take(5).mkString(" ")} …"
              else cs.mkString(" ")
            )
          ),
          " "
        )
    )
    val PATH(f, l) = code.p
    println(fansi.Color.DarkGray(s"———(${f.getOrElse("?")}:$l)"))

  def trace2 = println(stack.map(_.toForm).mkString("\n"))

  def trace =
    trace1
    trace2

  def modCode(f: List[ANY] => List[ANY]) =
    copy(code = ANY.FN(code.p, f(code.x)))

  def loadCode(x: List[ANY]) = modCode(x ++ _)

  def modLine(p: PATH, x: ANY) =
    copy(lines = lines + (p -> x))

  def modStack(f: Vector[ANY] => Vector[ANY]) =
    copy(stack = f(stack))

  def getLine(i: Int) = lines.get(PATH(code.p.f, i))

  def fnLine(i: Int) =
    val p = PATH(code.p.f, i)
    lines.get(p) match
      case Some(x) =>
        modLine(
          p,
          x match
            case ANY.STR(_) => x.iFN(i, this)
            case _          => x
        )
      case _ => this

  def loadLine(i: Int) =
    val env = fnLine(i)
    env.loadCode(env.getLine(i) match
      case Some(ANY.FN(_, x)) => x
      case _                  => List()
    )

  def push(x: ANY)           = modStack(_ :+ x)
  def pushs(xs: Vector[ANY]) = modStack(_ ++ xs)

  def execA(c: ANY) = c match
    case ANY.CMD(x) =>
      if x.startsWith("\\") && x.length > 1 then
        ANY.CMD(x.drop(1)).toFN(this).pipe(push)
      else ???
    case _ => push(c)

  def exec: ENV = code.x match
    case List() => this
    case c :: cs =>
      if eS || eV then trace1
      modCode(_ => cs).execA(c).tap(_.trace2).exec

object ENV:

  def run(o: (Boolean, Boolean, Boolean), f: FILE, l: String) =
    val (s, v, i) = o
    ENV(
      l.linesIterator.zipWithIndex.map { case (x, i) =>
        (PATH(f, i), ANY.STR(x))
      }.toMap,
      ANY.FN(PATH(f, 0), List()),
      eS = s,
      eV = v,
      eI = i
    )
      .loadLine(0)
      .exec
      .tap(env => if env.eS || env.eV || env.eI then env.trace)
