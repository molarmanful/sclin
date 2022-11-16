package sclin

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
    code: FN = FN(PATH(None, 0), List()),
    stack: ARRW[ANY] = Vector(),
    scope: Map[String, ANY] = Map(),
    gscope: TrieMap[String, ANY] = TrieMap(),
    ids: Map[String, PATH] = Map(),
    gids: TrieMap[String, PATH] = TrieMap(),
    arr: List[ARRW[ANY]] = List(),
    eS: Boolean = false,
    eV: Boolean = false,
    eI: Boolean = false
):

  def trace1: ENV =
    println(fansi.Color.DarkGray(s"———(${code.p})"))
    println(code.x match
      case List() => fansi.Color.Green("(EMPTY)")
      case c :: cs =>
        fansi.Str.join(
          Seq(
            fansi.Bold.On(fansi.Color.Yellow(c.toForm)),
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

  def trace2: ENV =
    println(fansi.Color.DarkGray("———>"))
    println(stack.map(_.toForm).mkString("\n"))
    this

  def trace: ENV = trace1.trace2

  def modCode(f: List[ANY] => List[ANY]): ENV =
    copy(code = FN(code.p, f(code.x)))

  def loadCode(x: List[ANY]): ENV = modCode(x ++ _)

  def modStack(f: ARRW[ANY] => ARRW[ANY]): ENV =
    copy(stack = f(stack))

  def getStack(i: Int): ANY = stack.applyOrElse(iStack(i), _ => UN)

  def iStack(i: Int): Int =
    val i1 = ~i
    if i1 < 0 then stack.length + i1 else i1

  def setArr(x: List[ARRW[ANY]]): ENV = copy(arr = x)

  def setLine(p: PATH, x: STR, y: ANY): ENV =
    lines += (p -> (x, y))
    this

  def setLineF(i: Int, x: ANY): ENV =
    val p = PATH(code.p.f, i)
    lines += (p -> (lines(p)._1, x))
    this

  def getLine(i: Int): Option[(ANY, ANY)] = lines.get(PATH(code.p.f, i))

  def getLineS(i: Int): ANY = getLine(i) match
    case Some(x, _) => x
    case _          => UN

  def getLineF(i: Int): ANY = getLine(i) match
    case Some(x, y) =>
      y match
        case _: FN => y
        case _     => x
    case _ => UN

  def fnLine(i: Int): ENV = getLine(i) match
    case Some(x, y) =>
      setLineF(
        i,
        y match
          case UN => x.lFN(i, this)
          case _  => y
      )
    case _ => this

  def loadLine(i: Int): ENV =
    fnLine(i)
    copy(code = getLineF(i) match
      case x: FN => x
      case _     => FN(code.p, List())
    )

  def getId(c: String): PATH = lines.find { case (_, (s, _)) =>
    s.x.trim.startsWith("#" + c)
  } match
    case Some(p, _) => p
    case _          => throw LinEx("ID", "unknown id")

  def optId(c: String): Option[PATH] = try Some(getId(c))
  catch e => None

  def addLocId(c: String): ENV =
    copy(ids = ids + (c -> getId(c)), scope = scope - c)

  def addGlobId(c: String): ENV =
    gids   += (c -> getId(c))
    gscope -= c
    this

  def addLoc(k: String, v: ANY): ENV = copy(scope = scope + (k -> v))

  def addGlob(k: String, v: ANY): ENV =
    gscope += (k -> v)
    this

  def push(x: ANY): ENV         = modStack(_ :+ x)
  def pushs(xs: ARRW[ANY]): ENV = modStack(_ ++ xs)

  def arg(n: Int, f: (ARRW[ANY], ENV) => ENV) =
    if stack.length < n then throw LinEx("ST_LEN", s"stack length < $n")
    else
      val (xs, ys) = stack.splitAt(stack.length - n)
      f(ys, modStack(_ => xs))

  def mods(n: Int, f: ARRW[ANY] => ARRW[ANY]): ENV =
    arg(n, (xs, env) => env.pushs(f(xs)))

  def modx(n: Int, f: ARRW[ANY] => ANY): ENV = mods(n, xs => Vector(f(xs)))

  def arg1(f: (ANY, ENV) => ENV): ENV =
    arg(1, { case (Vector(x), env) => f(x, env); case _ => ??? })
  def arg2(f: (ANY, ANY, ENV) => ENV): ENV =
    arg(2, { case (Vector(x, y), env) => f(x, y, env); case _ => ??? })
  def arg3(f: (ANY, ANY, ANY, ENV) => ENV): ENV =
    arg(3, { case (Vector(x, y, z), env) => f(x, y, z, env); case _ => ??? })

  def mods1(f: ANY => ARRW[ANY]): ENV =
    mods(1, { case Vector(x) => f(x); case _ => ??? })
  def mods2(f: (ANY, ANY) => ARRW[ANY]): ENV =
    mods(2, { case Vector(x, y) => f(x, y); case _ => ??? })
  def mods3(f: (ANY, ANY, ANY) => ARRW[ANY]): ENV =
    mods(3, { case Vector(x, y, z) => f(x, y, z); case _ => ??? })

  def mod1(f: ANY => ANY): ENV =
    modx(1, { case Vector(x) => f(x); case _ => ??? })
  def mod2(f: (ANY, ANY) => ANY): ENV =
    modx(2, { case Vector(x, y) => f(x, y); case _ => ??? })
  def mod3(f: (ANY, ANY, ANY) => ANY): ENV =
    modx(3, { case Vector(x, y, z) => f(x, y, z); case _ => ??? })

  def vec1(f: ANY => ANY): ENV        = mod1(_.vec1(f))
  def vec2(f: (ANY, ANY) => ANY): ENV = mod2(_.vec2(_, f))

  def num1(f: NUMF => NUMF): ENV                    = mod1(_.num1(f))
  def num1(f: NUMF => NUMF, e: String): ENV         = mod1(_.num1(f, e))
  def num2(f: (NUMF, NUMF) => NUMF): ENV            = mod2(_.num2(_, f))
  def num2(f: (NUMF, NUMF) => NUMF, e: String): ENV = mod2(_.num2(_, f, e))
  def num2q(f: (NUMF, NUMF) => Iterable[NUMF]): ENV = mod2(_.num2q(_, f))
  def num2a(f: (NUMF, NUMF) => Iterable[NUMF]): ENV = mod2(_.num2a(_, f))

  def str1a(f: String => Iterable[String]): ENV           = mod1(_.str1a(f))
  def str1(f: String => String): ENV                      = mod1(_.str1(f))
  def str2(f: (String, String) => String): ENV            = mod2(_.str2(_, f))
  def str2q(f: (String, String) => Iterable[String]): ENV = mod2(_.str2q(_, f))
  def str2a(f: (String, String) => Iterable[String]): ENV = mod2(_.str2a(_, f))

  def strnum(f: (String, NUMF) => String): ENV = mod2(_.strnum(_, f))
  def strnumq(f: (String, NUMF) => Iterable[String]): ENV = mod2(
    _.strnumq(_, f)
  )
  def strnuma(f: (String, NUMF) => Iterable[String]): ENV = mod2(
    _.strnuma(_, f)
  )

  def execA(c: ANY): ENV = c match
    case CMD(x)       => this.cmd(x)
    case TRY(b, x, e) => if b then push(x) else throw e
    case _            => push(c)

  @tailrec final def exec: ENV = code.x match
    case List() => this
    case c :: cs =>
      if eS then print("\u001b[2J\u001b[;H")
      if eS || eV then trace1
      try
        return modCode(_ => cs)
          .execA(c)
          .tap(e => if eS || eV then e.trace2)
          .pipe(e =>
            if eS then
              print(fansi.Color.DarkGray("———? "))
              io.StdIn.readLine match
                case "v" => e.copy(eS = false, eV = true)
                case _   => e
            else e
          )
          .tap(_ => if eS then print("\u001b[2J\u001b[;H"))
          .exec
      catch
        case e: LinEx => throw e.toLinERR(this)
        case e: java.lang.StackOverflowError =>
          throw LinEx("REC", "stack overflow").toLinERR(this)

/** Frontend for `ENV`. */
object ENV:

  def run(
      l: String,
      f: FILE = None,
      o: (Boolean, Boolean, Boolean) = (false, false, false)
  ): ENV =
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

  def docRun(l: String): Unit =
    val s = ENV.run(l).stack.map(_.toForm).mkString(" ")
    println("-> " + s)
