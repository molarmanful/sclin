package sclin

import monix.execution.Scheduler
import scala.annotation.*
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.HashMap
import scala.util.chaining.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import ANY.*

/** A single step in the execution of a lin program.
  *
  * @param lines
  *   cache of all lines being read
  * @param code
  *   queue of data to evaluate
  * @param stack
  *   current data stack
  * @param curPC
  *   current PATH and executing CMD
  * @param calls
  *   current call stack
  * @param scope
  *   current scope
  * @param gscope
  *   global scope
  * @param arr
  *   queue of strucures being constructed
  * @param flags
  *   CLI flags
  * @param cflag
  *   function for (de)coloring output
  * @param ioSched
  *   IO-specific thread-pool
  */
case class ENV(
    lines: TrieMap[PATH, (STR, ANY)] = TrieMap(),
    code: FN = FN(PATH(None, 0), LazyList()),
    stack: ARRW[ANY] = Vector(),
    curPC: (PATH, ANY) = (PATH(None, 0), UN),
    calls: SEQW[(PATH, ANY)] = LazyList(),
    scope: SCOPE = HashMap(),
    gscope: GSCOPE = TrieMap(),
    ids: IDS = HashMap(),
    gids: GIDS = TrieMap(),
    arr: List[ARRW[ANY]] = List(),
    flags: Flags = Flags(),
    cflag: fansi.Attrs => fansi.Attrs = _ => fansi.Attrs(),
    ioSched: Scheduler = ioSched
):

  def trace1: ENV =
    println(cflag(fansi.Color.DarkGray)(s"———(${code.p})"))
    println:
      code.x match
        case LazyList() => cflag(fansi.Color.Green)("(EMPTY)")
        case c #:: cs =>
          fansi.Str.join(
            Seq(
              cflag(fansi.Bold.On ++ fansi.Color.Yellow)(c.toForm),
              cflag(fansi.Color.DarkGray):
                if cs.length > 5 then
                  s"${cs.take(5).map(_.toForm).mkString(" ")} …"
                else cs.map(_.toForm).mkString(" ")
            ),
            " "
          )
    this

  def trace2: ENV =
    println(cflag(fansi.Color.DarkGray)("———>"))
    println(stack.map(_.toForm).mkString("\n"))
    this

  def trace: ENV = trace1.trace2

  def modCode(f: LazyList[ANY] => LazyList[ANY]): ENV =
    copy(code = FN(code.p, f(code.x)))

  def loadCode(x: LazyList[ANY]): ENV = modCode(x ++ _)

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
        case UN => x
        case _  => y
    case _ => UN

  def fnLine(i: Int): ENV = getLine(i) match
    case Some(x, y) =>
      val y1 = y match
        case _: FN => y
        case _     => x.lFN(i, this)
      setLineF(i, y1)
    case _ => this

  def loadLine(i: Int): ENV =
    fnLine(i)
    getLineF(i) match
      case x: FN => copy(code = x)
      case _     => this

  def getId(c: String): PATH = lines.find:
    case (_, (s, _)) => s.x.trim.startsWith("#" + c)
  match
    case Some(p, _) => p
    case _          => throw LinEx("ID", s"unknown id \"$c\"")

  def optId(c: String): Option[PATH] = Try(getId(c)).toOption

  def addLocId(c: String): ENV =
    copy(ids = ids + (c -> getId(c)), scope = scope - c)

  def addGlobId(c: String): ENV =
    gids   += c -> getId(c)
    gscope -= c
    this

  def addLoc(k: String, v: ANY): ENV = copy(scope = scope + (k -> v))

  def addGlob(k: String, v: ANY): ENV =
    gscope += (k -> v)
    this

  def getLoc(k: String): Option[ANY] =
    if scope.contains(k) then scope.get(k)
    else if ids.contains(k) then ids.get(k).map(_.l.pipe(getLineS))
    else getGlob(k)

  def getGlob(k: String): Option[ANY] =
    if gscope.contains(k) then gscope.get(k)
    else if gids.contains(k) then gids.get(k).map(_.l.pipe(getLineS))
    else None

  def addCall(): ENV = copy(calls = curPC #:: calls)

  def setCur(c: ANY): ENV = copy(curPC = (code.p, c))

  def push(x: ANY): ENV         = modStack(_ :+ x)
  def pushs(xs: ARRW[ANY]): ENV = modStack(_ ++ xs)

  def arg(n: Int)(f: (ARRW[ANY], ENV) => ENV): ENV =
    if stack.length < n then throw LinEx("ST_LEN", s"stack length < $n")
    else
      val (xs, ys) = stack.splitAt(stack.length - n)
      f(ys, modStack(_ => xs))

  def mods(n: Int)(f: ARRW[ANY] => ARRW[ANY]): ENV =
    arg(n)((xs, env) => env.pushs(f(xs)))

  def modx(n: Int)(f: ARRW[ANY] => ANY): ENV = mods(n)(xs => Vector(f(xs)))

  def arg1(f: (ANY, ENV) => ENV): ENV =
    arg(1):
      case (Vector(x), env) => f(x, env)
  def arg2(f: (ANY, ANY, ENV) => ENV): ENV =
    arg(2):
      case (Vector(x, y), env) => f(x, y, env)
  def arg3(f: (ANY, ANY, ANY, ENV) => ENV): ENV =
    arg(3):
      case (Vector(x, y, z), env) => f(x, y, z, env)

  def mods1(f: ANY => ARRW[ANY]): ENV =
    mods(1):
      case Vector(x) => f(x)
  def mods2(f: (ANY, ANY) => ARRW[ANY]): ENV =
    mods(2):
      case Vector(x, y) => f(x, y)
  def mods3(f: (ANY, ANY, ANY) => ARRW[ANY]): ENV =
    mods(3):
      case Vector(x, y, z) => f(x, y, z)

  def mod1(f: ANY => ANY): ENV =
    modx(1):
      case Vector(x) => f(x)
  def mod2(f: (ANY, ANY) => ANY): ENV =
    modx(2):
      case Vector(x, y) => f(x, y)
  def mod3(f: (ANY, ANY, ANY) => ANY): ENV =
    modx(3):
      case Vector(x, y, z) => f(x, y, z)

  def vec1(f: ANY => ANY): ENV             = mod1(_.vec1(f))
  def vec2(f: (ANY, ANY) => ANY): ENV      = mod2(_.vec2(_)(f))
  def vec3(f: (ANY, ANY, ANY) => ANY): ENV = mod3(_.vec3(_, _)(f))

  def num1(f: Double => Double, g: NUMF => NUMF): ENV = mod1(_.num1(f, g))
  def num1(f: Double => Double, g: NUMF => NUMF, e: String): ENV =
    mod1(_.num1(f, g, e))
  def num2(f: (Double, Double) => Double, g: (NUMF, NUMF) => NUMF): ENV =
    mod2(_.num2(_, f, g))
  def num2(
      f: (Double, Double) => Double,
      g: (NUMF, NUMF) => NUMF,
      e: String
  ): ENV =
    mod2(_.num2(_, f, g, e))
  def num2q(f: (NUMF, NUMF) => Iterable[NUMF]): ENV = mod2(_.num2q(_, f))
  def num2a(f: (NUMF, NUMF) => Iterable[NUMF]): ENV = mod2(_.num2a(_, f))

  def str1a(f: String => Iterable[String]): ENV           = mod1(_.str1a(f))
  def str1(f: String => String): ENV                      = mod1(_.str1(f))
  def str2(f: (String, String) => String): ENV            = mod2(_.str2(_, f))
  def str2q(f: (String, String) => Iterable[String]): ENV = mod2(_.str2q(_, f))
  def str2a(f: (String, String) => Iterable[String]): ENV = mod2(_.str2a(_, f))

  def strnum(f: (String, NUMF) => String): ENV = mod2(_.strnum(_, f))
  def strnumq(f: (String, NUMF) => Iterable[String]): ENV = mod2:
    _.strnumq(_, f)
  def strnuma(f: (String, NUMF) => Iterable[String]): ENV = mod2:
    _.strnuma(_, f)

  def execA(c: ANY): ENV = c match
    case CMD(x)          => this.cmd(x)
    case _: TASK         => push(c.toFUT)
    case _: FUT          => push(c).await
    case TRY(Success(x)) => push(x)
    case TRY(Failure(e)) => throw e
    case ERR(e)          => throw e
    case _               => push(c)

  @tailrec final def exec: ENV = code.x match
    case LazyList() => this
    case c #:: cs =>
      if flags.s then print("\u001b[2J\u001b[;H")
      if flags.s || flags.v then trace1
      val env = setCur(c).modCode(_ => cs)
      val env1 =
        try env.execA(c)
        catch
          case e: LinEx => throw e.toLinERR(env)
          case _: java.lang.StackOverflowError =>
            throw LinEx("REC", "stack overflow").toLinERR(env)
      env1
        .tap(e => if flags.s || flags.v then e.trace2)
        .pipe: e =>
          if flags.s then
            print(cflag(fansi.Color.DarkGray)("———? "))
            io.StdIn.readLine match
              case "v" => e.copy(flags = flags.copy(s = false, v = true))
              case _   => e
          else e
        .tap(_ => if flags.s then print("\u001b[2J\u001b[;H"))
        .exec

/** Frontend for `ENV`. */
object ENV:

  def run(
      l: String,
      f: FILE = None,
      flags: Flags = Flags(),
      cflag: fansi.Attrs => fansi.Attrs = _ => fansi.Attrs()
  ): ENV =
    ENV(
      TrieMap.from:
        l.linesIterator.zipWithIndex.map:
          case (x, i) => (PATH(f, i), (STR(x), UN))
      ,
      FN(PATH(f, 0), LazyList()),
      flags = flags,
      cflag = cflag
    )
      .loadLine(0)
      .exec
      .tap(env => if env.flags.s || env.flags.v || env.flags.i then env.trace)

  def docRun(l: String): Unit =
    val s = ENV.run(l).stack.map(_.toForm).mkString(" ")
    println("-> " + s)

  lazy val ioSched: Scheduler = Scheduler.io("io")
