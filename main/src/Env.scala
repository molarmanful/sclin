import org.apfloat.{ApfloatMath => Ap, FixedPrecisionApfloatHelper => Afp, _}
import scala.util.chaining._
import ANY._

/** A single step in the execution of a lin program.
  *
  * @param lines
  *   cache of all lines being read
  * @param code
  *   queue of data to evaluate
  * @param stack
  *   current data stack
  * @param scope
  *   current scope
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
    lines: LINESW = Map(),
    code: FN,
    stack: ARRW = Vector(),
    scope: MAPW = Map(),
    arr: List[ARRW] = List(),
    fixp: Afp = Afp(100),
    // rng: Uniform[Long],
    eS: Boolean = false,
    eV: Boolean = false,
    eI: Boolean = false
):

  /** Prints debug trace header. */
  def trace1: ENV =
    println(fansi.Color.DarkGray("———>"))
    println(code.x match
      case List() => fansi.Color.Green("(EMPTY)")
      case c :: cs =>
        fansi.Str.join(
          Seq(
            fansi.Bold.On(fansi.Color.Yellow(c.toString)),
            fansi.Color.DarkGray(
              if cs.length > 5 then
                s"${cs.take(5).map(_.toForm).mkString(" ")} …"
              else cs.map(_.toForm).mkString(" ")
            )
          ),
          " "
        )
    )
    println(fansi.Color.DarkGray(s"———(${code.p})"))
    this

  /** Prints debug trace stack. */
  def trace2: ENV =
    println(stack.map(_.toForm).mkString("\n"))
    this

  /** Prints debug trace. */
  def trace: ENV = trace1.trace2

  /** Modifies `code` with function.
    *
    * @param f
    *   function that modifies `code`
    */
  def modCode(f: List[ANY] => List[ANY]): ENV =
    copy(code = FN(code.p, f(code.x)))

  /** Prepends list to `code`.
    *
    * @param x
    *   list to prepend
    */
  def loadCode(x: List[ANY]): ENV = modCode(x ++ _)

  /** Modifies `stack` with function.
    *
    * @param f
    *   function that modifies `stack`
    */
  def modStack(f: ARRW => ARRW): ENV =
    copy(stack = f(stack))

  /** Indexes stack starting from top.
    *
    * @param i
    *   index of item to retrieve
    */
  def getStack(i: Int): ANY = stack.applyOrElse(stack.length - 1 - i, _ => UN)

  /** Sets `arr`.
    *
    * @param x
    *   new value of `arr`
    */
  def setArr(x: List[ARRW]): ENV = copy(arr = x)

  /** Modifies line at path in `lines`.
    *
    * @param p
    *   line path to modify
    * @param x
    *   new line
    */
  def setLine(p: PATH, x: ANY): ENV =
    copy(lines = lines + (p -> x))

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
        setLine(
          p,
          x match
            case _: STR => x.iFN(i, this)
            case _      => x
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
      case Some(FN(_, x)) => x
      case _              => List()
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
  def pushs(xs: ARRW): ENV = modStack(_ ++ xs)

  /** Pops top `n` `ANY`s and modifies `ENV` accordingly.
    *
    * @param n
    *   number of `ANY`s to pop
    * @param f
    *   function to modify `ENV` with
    */
  def arg(n: Int, f: (ARRW, ENV) => ENV) =
    if stack.length < n then
      throw LinERR(code.p, "ST_LEN", s"stack length < $n")
    else
      val (xs, ys) = stack.splitAt(stack.length - n)
      f(ys, modStack(_ => xs))

  /** Modifies top `n` `ANY`s purely.
    *
    * @param n
    *   number of `ANY`s to pop
    * @param f
    *   function to modify top `ANY` with
    */
  def mods(n: Int, f: ARRW => ARRW): ENV =
    arg(n, (xs, env) => env.pushs(f(xs)))

  /** Modifies top `n` `ANY`s purely into one `ANY`.
    *
    * @param n
    *   number of `ANY`s to pop
    * @param f
    *   function to modify top `ANY` with
    */
  def modx(n: Int, f: ARRW => ANY): ENV = mods(n, xs => Vector(f(xs)))

  def arg1(f: (ANY, ENV) => ENV): ENV =
    arg(1, { case (Vector(x), env) => f(x, env); case _ => ??? })

  def arg2(f: (ANY, ANY, ENV) => ENV): ENV =
    arg(2, { case (Vector(x, y), env) => f(x, y, env); case _ => ??? })

  def arg3(f: (ANY, ANY, ANY, ENV) => ENV): ENV =
    arg(3, { case (Vector(x, y, z), env) => f(x, y, z, env); case _ => ??? })

  def mods1(f: ANY => ARRW): ENV =
    mods(1, { case Vector(x) => f(x); case _ => ??? })

  def mods2(f: (ANY, ANY) => ARRW): ENV =
    mods(2, { case Vector(x, y) => f(x, y); case _ => ??? })

  def mods3(f: (ANY, ANY, ANY) => ARRW): ENV =
    mods(3, { case Vector(x, y, z) => f(x, y, z); case _ => ??? })

  def mod1(f: ANY => ANY): ENV =
    modx(1, { case Vector(x) => f(x); case _ => ??? })

  def mod2(f: (ANY, ANY) => ANY): ENV =
    modx(2, { case Vector(x, y) => f(x, y); case _ => ??? })

  def mod3(f: (ANY, ANY, ANY) => ANY): ENV =
    modx(3, { case Vector(x, y, z) => f(x, y, z); case _ => ??? })

  def num1(f: NUMF => NUMF): ENV =
    mod1(
      _.vec1(x =>
        try NUM(f(x.toNUM.x))
        catch
          case _: ArithmeticException => UN
          case e                      => throw LinERR(code.p, "MATH", e.getMessage)
      )
    )

  def num2(f: (NUMF, NUMF) => NUMF): ENV =
    mod2(
      _.vec2(_)((x, y) =>
        try NUM(f(x.toNUM.x, y.toNUM.x))
        catch
          case _: ArithmeticException => UN
          case e                      => throw LinERR(code.p, "MATH", e.getMessage)
      )
    )

  /** Executes `CMD`s and pushes other `ANY`s.
    *
    * @param c
    *   `ANY` to evaluate
    */
  def execA(c: ANY): ENV = c match
    case CMD(x) =>
      x match
        case s"\$y" if y != ""   => push(CMD(y).toFN(this))
        case s"#$y" if y != ""   => this
        case s"$$$y" if y != ""  => ???
        case s"=$$$y" if y != "" => ???
        case _                   => this.cmd(x)
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
        (PATH(f, i), STR(x))
      }.toMap,
      FN(PATH(f, 0), List()),
      eS = s,
      eV = v,
      eI = i
    )
      .loadLine(0)
      .exec
      .tap(env => if env.eS || env.eV || env.eI then env.trace)
