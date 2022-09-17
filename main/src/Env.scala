import scala.util.chaining._
import spire.algebra._
import spire.implicits._
import spire.math._
import spire.random._

/** A single step in the execution of a lin program.
  *
  * @param lines
  *   cache of all lines being read
  * @param code
  *   queue of data to evaluate
  * @param stack
  *   current data stack
  * @param scope
  *   variables
  * @param arr
  *   queue of strucures being constructed
  * @param eS
  *   step mode
  * @param eV
  *   verbose mode
  * @param eI
  *   implicit mode
  */
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

  /** Prints debug trace header. */
  def trace1: Unit =
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

  /** Prints debug trace stack. */
  def trace2: Unit = println(stack.map(_.toForm).mkString("\n"))

  /** Prints debug trace. */
  def trace: Unit =
    trace1
    trace2

  /** Modifies `code` with function.
    *
    * @param f
    *   function that modifies `code`
    */
  def modCode(f: List[ANY] => List[ANY]): ENV =
    copy(code = ANY.FN(code.p, f(code.x)))

  /** Prepends list to `code`.
    *
    * @param x
    *   list to prepend
    */
  def loadCode(x: List[ANY]): ENV = modCode(x ++ _)

  /** Modifies line at path in `lines`.
    *
    * @param p
    *   line path to modify
    * @param x
    *   new line
    */
  def modLine(p: PATH, x: ANY): ENV =
    copy(lines = lines + (p -> x))

  /** Modifies `stack` with function.
    *
    * @param f
    *   function that modifies `stack`
    */
  def modStack(f: Vector[ANY] => Vector[ANY]): ENV =
    copy(stack = f(stack))

  /** Gets line at number in `lines`.
    *
    * @param i
    *   line number to retrieve
    * @return
    */
  def getLine(i: Int): Option[ANY] = lines.get(PATH(code.p.f, i))

  /** Converts line at number in `lines` to `FN`.
    *
    * @param i
    *   line number to convert
    */
  def fnLine(i: Int): ENV =
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

  /** Converts line at number in `lines` to `FN` and pushes to `code`.
    *
    * @param i
    *   line number to load
    */
  def loadLine(i: Int): ENV =
    val env = fnLine(i)
    env.loadCode(env.getLine(i) match
      case Some(ANY.FN(_, x)) => x
      case _                  => List()
    )

  /** Pushes `ANY` to `stack`.
    *
    * @param x
    *   `ANY` to push
    */
  def push(x: ANY): ENV = modStack(_ :+ x)

  /** Pushes `ANY`s in vector to `stack`.
    *
    * @param xs
    *   vector of `ANY`s to push
    */
  def pushs(xs: Vector[ANY]): ENV = modStack(_ ++ xs)

  /** Executes `CMD`s and pushes other `ANY`s.
    *
    * @param c
    *   item to evaluate
    */
  def execA(c: ANY): ENV = c match
    case ANY.CMD(x) =>
      if x.startsWith("\\") && x.length > 1 then
        ANY.CMD(x.drop(1)).toFN(this).pipe(push)
      else ???
    case _ => push(c)

  /** Executes an `ENV`. */
  def exec: ENV = code.x match
    case List() => this
    case c :: cs =>
      if eS || eV then trace1
      modCode(_ => cs).execA(c).tap(e => if eS || eV then e.trace2).exec

/** Frontend for `ENV`. */
object ENV:

  /** Creates a new `ENV` around given code and executes it.
    *
    * @param o
    *   debug flags
    * @param f
    *   file path
    * @param l
    *   code to run
    */
  def run(o: (Boolean, Boolean, Boolean), f: FILE, l: String): ENV =
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
