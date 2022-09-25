import org.apfloat.{ApfloatMath => Ap, FixedPrecisionApfloatHelper => Afp, _}
import scala.annotation._
import scala.collection.concurrent.TrieMap
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
  * @param gscope
  *   global scope
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
    lines: TrieMap[PATH, (STR, ANY)] = TrieMap(),
    code: FN,
    stack: ARRW = Vector(),
    scope: Map[String, ANY] = Map(),
    gscope: TrieMap[String, ANY] = TrieMap(),
    arr: List[ARRW] = List(),
    fixp: Afp = Afp(100),
    eS: Boolean = false,
    eV: Boolean = false,
    eI: Boolean = false
):

  /** Prints debug trace header. */
  def trace1: ENV =
    println(fansi.Color.DarkGray(s"———(${code.p})"))
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
    this

  /** Prints debug trace stack. */
  def trace2: ENV =
    println(fansi.Color.DarkGray("———>"))
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
  def getStack(i: Int): ANY = stack.applyOrElse(iStack(i), _ => UN)

  def iStack(i: Int): Int =
    val i1 = ~i
    if i1 < 0 then stack.length + i1 else i1

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
  def setLine(p: PATH, x: STR, y: ANY): ENV =
    lines += (p -> (x, y))
    this

  def setLineF(i: Int, x: ANY): ENV =
    val p = PATH(code.p.f, i)
    lines += (p -> (lines(p)._1, x))
    this

  /** Gets line at number in `lines`.
    *
    * @param i
    *   line number to retrieve
    * @return
    */
  def getLine(i: Int): Option[(ANY, ANY)] = lines.get(PATH(code.p.f, i))

  /** Gets STR part of line at number in `lines`.
    *
    * @param i
    *   line number to retrieve
    * @return
    */
  def getLineS(i: Int): ANY = getLine(i) match
    case Some(x, _) => x
    case _          => UN

  /** Gets FN part of line at number in `lines`.
    *
    * @param i
    *   line number to retrieve
    * @return
    */
  def getLineF(i: Int): ANY = getLine(i) match
    case Some(x, y) =>
      y match
        case _: FN => y
        case _     => x
    case _ => UN

  /** Caches line at number in `lines` as `FN`.
    *
    * @param i
    *   line number to cache
    */
  def fnLine(i: Int): ENV = getLine(i) match
    case Some(x, y) =>
      setLineF(
        i,
        y match
          case UN => x.lFN(i, this)
          case _  => y
      )
    case _ => this

  /** Converts line at number in `lines` to `FN` and pushes to `code`.
    *
    * @param i
    *   line number to load
    */
  def loadLine(i: Int): ENV =
    fnLine(i)
    copy(code = getLineF(i) match
      case x: FN => x
      case _     => FN(code.p, List())
    )

  /** Adds variable to `scope`.
    * @param k
    *   variable name
    * @param v
    *   variable value
    */
  def addLoc(k: String, v: ANY): ENV = copy(scope = scope + (k -> v))

  /** Adds variable to `gscope`.
    * @param k
    *   variable name
    * @param v
    *   variable value
    */
  def addGlob(k: String, v: ANY): ENV =
    gscope += (k -> v)
    this

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

  def arg(n: Int, f: (ARRW, ENV) => ENV) =
    if stack.length < n then
      throw LinERR(code.p, "ST_LEN", s"stack length < $n")
    else
      val (xs, ys) = stack.splitAt(stack.length - n)
      f(ys, modStack(_ => xs))

  def mods(n: Int, f: ARRW => ARRW): ENV =
    arg(n, (xs, env) => env.pushs(f(xs)))

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

  def num1(f: NUMF => NUMF): ENV         = mod1(_.num1(this, f))
  def num2(f: (NUMF, NUMF) => NUMF): ENV = mod2(_.num2(this, _, f))

  def str1(f: String => String): ENV           = mod1(_.str1(f))
  def str2(f: (String, String) => String): ENV = mod2(_.str2(_, f))

  def strnum(f: (String, NUMF) => String): ENV = mod2(_.strnum(_, f))

  /** Executes `CMD`s and pushes other `ANY`s.
    *
    * @param c
    *   `ANY` to evaluate
    */
  def execA(c: ANY): ENV = c match
    case CMD(x) =>
      x match
        case s"\$y" if y != ""                          => push(CMD(y).toFN(this))
        case s"#$y" if y != ""                          => this
        case s"$$$$$y" if y != "" && gscope.contains(y) => push(gscope(y))
        case s"=$$$$$y" if y != ""                      => arg1((v, env) => env.addGlob(y, v))
        case s"$$$y" if y != "" && scope.contains(y)    => push(scope(y))
        case s"=$$$y" if y != ""                        => arg1((v, env) => env.addLoc(y, v))
        case x if scope.contains(x)                     => push(scope(x)).eval
        case x if gscope.contains(x)                    => push(gscope(x)).eval
        case _                                          => this.cmd(x)
    case _ => push(c)

  /** Executes an `ENV`. */
  @tailrec
  final def exec: ENV = code.x match
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
      TrieMap.from(l.linesIterator.zipWithIndex.map { case (x, i) =>
        (PATH(f, i), (STR(x), UN))
      }),
      FN(PATH(f, 0), List()),
      eS = s,
      eV = v,
      eI = i
    )
      .loadLine(0)
      .exec
      .tap(env => if env.eS || env.eV || env.eI then env.trace)
