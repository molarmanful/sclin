package sclin

import better.files.*
import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.immutable.VectorMap
import scala.util.chaining.*
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import spire.implicits.*
import spire.math.*
import upickle.default.*
import ANY.*
import Lambda.*

extension (env: ENV)

  def SIG_1f1(f: ANY)(a: ANY): ANY     = env.evalA1(Vector(a), f)
  def SIG_1f_(f: ANY)(a: ANY): ANY     = SIG_1f1(f)(a).pipe(_ => a)
  def SIG_1fb(f: ANY)(a: ANY): Boolean = SIG_1f1(f)(a).toBool

  def SIG_2f1(f: ANY)(a: ANY, b: ANY): ANY = env.evalA1(Vector(a, b), f)
  def SIG_2f_(f: ANY)(a: ANY, b: ANY): (ANY, ANY) =
    SIG_2f1(f)(a, b).pipe(_ => (a, b))
  def SIG_2fb(f: ANY)(a: ANY, b: ANY): Boolean    = SIG_2f1(f)(a, b).toBool
  def SIG_2f2(f: ANY)(a: ANY, b: ANY): (ANY, ANY) = env.evalA2(Vector(a, b), f)

  def SIG_1y2f1(f: ANY)(a: ANY, b: (ANY, ANY)): ANY =
    env.evalA1(Vector(b._1, a, b._2), f)
  def SIG_2y1f1(f: ANY)(a: (ANY, ANY), b: ANY): ANY =
    env.evalA1(Vector(a._1, a._2, b), f)
  def SIG_2x2fb(f: ANY)(a: (ANY, ANY), b: (ANY, ANY)): Boolean =
    env
      .evalA1(Vector(Vector(a._1, a._2).toARR, Vector(b._1, b._2).toARR), f)
      .toBool

  def cmd(x: String): ENV = x match
    case s"#$k" if k != "" => env
    case s"\$k" if k != "" => env.push(CMD(k).toFN(env))
    case s"`$k" if k != "" =>
      @tailrec def loop(
          n: Int = env.code.p.l + 1,
          res: ARRW[String] = Vector()
      ): (ARRW[String], Int) =
        env.lines.get(PATH(env.code.p.f, n)) match
          case Some(STR(s), _) if !s.trim.startsWith(k) =>
            loop(n + 1, res :+ s)
          case _ => (res, n)
      val (x, i) = loop()
      env.push(STR(x.mkString("\n"))).push(NUM(i)).evalLine
    case "`"                   => env
    case s"=$$$$$k" if k != "" => env.arg1((v, env) => env.addGlob(k, v))
    case s"=$$$k" if k != ""   => env.arg1((v, env) => env.addLoc(k, v))
    case s"$$$$$k" if k != "" =>
      env.getGlob(k) match
        case None    => env.cmd1(x)
        case Some(v) => env.push(v)
    case s"$$$k" if k != "" =>
      env.getLoc(k) match
        case None    => env.cmd1(x)
        case Some(v) => env.push(v)
    case _ =>
      if env.code.s.contains(x) then env.push(env.code.s(x)).eval
      else if env.ids.contains(x) then env.push(NUM(env.ids(x).l)).evalLine
      else if env.gscope.contains(x) then env.push(env.gscope(x)).eval
      else if env.gids.contains(x) then env.push(NUM(env.gids(x).l)).evalLine
      else env.cmd1(x)

  def eval: ENV = env.arg1: (x, env) =>
    x match
      case f: FN =>
        env.code.x match
          case LazyList() => env.copy(code = f)
          case _          => evale
      case _ => env.push(x).envFN.eval
  def evale: ENV = env.arg1: (x, env) =>
    x match
      case f: FN => env.modStack(_ => env.copy(code = f).addCall().exec.stack)
      case _     => env.push(x).envFN.evale
  def evalS(x: ARRW[ANY], f: ANY): ARRW[ANY] =
    env.modStack(_ => x :+ f).evale.stack
  def evalA1(x: ARRW[ANY], f: ANY): ANY =
    env.modStack(_ => x :+ f).evale.getStack(0)
  def evalA2(x: ARRW[ANY], f: ANY): (ANY, ANY) =
    val env1 = env.modStack(_ => x :+ f).evale
    (env1.getStack(1), env1.getStack(0))
  def quar: ENV =
    env.arg1((x, env) => x.vec1(env.push(_).evale.getStack(0)).pipe(env.push))

  def startFN: ENV =
    val l = Lambda(env.code.x).loop
    val (cs, c) = l.ys match
      case cs :+ c if l.n <= 0 => (cs, c)
      case _                   => (l.ys, CMD(")"))
    env.modCode(_ => l.xs).push(env.code.copy(x = cs)).execA(c)

  def evalLine: ENV = env.arg1: (x, env) =>
    val i    = x.toInt
    val env1 = env.fnLine(i)
    env1.push(env1.getLineF(i)).eval
  def getLNum: ENV = env.push(NUM(env.code.p.l))
  def getLFile: ENV = env.push:
    env.code.p.f match
      case Some(x) => STR(x.toString)
      case _       => UN
  def getLns: ENV    = env.push(env.lines.toSeq.sortBy(_._1._2).map(_._2._1).toARR)
  def evalLRel: ENV  = getLNum.add.evalLine
  def evalLHere: ENV = env.push(NUM(0)).evalLRel
  def evalLNext: ENV = env.push(NUM(1)).evalLRel
  def evalLPrev: ENV = env.push(NUM(-1)).evalLRel
  def getLn: ENV     = env.mod1(n => env.getLineS(n.toNUM.x.intValue))
  def getLRel: ENV   = getLNum.add.getLn
  def getLHere: ENV  = env.push(NUM(0)).getLRel
  def getLNext: ENV  = env.push(NUM(1)).getLRel
  def getLPrev: ENV  = env.push(NUM(-1)).getLRel
  def evalAnd: ENV =
    env.arg2((x, f, env) => if x.toBool then env.push(f).eval else env)
  def evalAnd$ : ENV = env.arg2: (x, f, env) =>
    val env1 = env.push(x)
    if x.toBool then env1.push(f).eval else env1
  def evalOr: ENV =
    env.arg2((x, f, env) => if x.toBool then env else env.push(f).eval)
  def evalOr$ : ENV = env.arg2: (x, f, env) =>
    val env1 = env.push(x)
    if x.toBool then env1 else env1.push(f).eval
  def evalIf: ENV =
    env.arg3((x, f, g, env) => env.push(if x.toBool then f else g).eval)
  def evalIf$ : ENV =
    env.arg1: (f, env) =>
      f.toMAP.x.find:
        case (k, _) => env.push(k).evale.getStack(0).toBool
      match
        case Some(_, v) => env.push(v).eval
        case _          => env
  def evalTimes: ENV =
    env.arg2: (f, n, env) =>
      @tailrec def loop(env: ENV, n: Int): ENV =
        if n > 0 then loop(env.push(f).evale, n - 1)
        else env
      loop(env, n.toInt)
  def evalTry: ENV = env.arg2: (f, g, env) =>
    try env.push(f).evale
    catch case e => env.pushs(Vector(e.toERRW(env), g)).quar.pop
  def evalTRY: ENV = env.arg1: (x, env) =>
    env.push(x.vec1(f => Try(env.push(f).quar.getStack(0)).toTRY))
  def throwERR: ENV = env.arg1((x, _) => throw x.toERR.x)
  def evalArrSt: ENV = env.arg2: (x, f, env) =>
    env.push(env.push(x).unwrap$.push(f).evale.stack.toARR.matchType(x))
  def evalStArr: ENV = env.arg1((f, env) => env.wrap$$.push(f).quar.unwrap$)
  def end: ENV       = env.modCode(_ => LazyList())
  def evalEnd: ENV   = env.end.eval

  def startARR: ENV = env.setArr(env.stack :: env.arr).clr
  def endARR: ENV = env.arr match
    case List()  => env
    case x :: xs => env.setArr(xs).modStack(_ => x).push(env.stack.toARR)
  def endMAP: ENV = endARR.envMAP

  def getscope: ENV = env.push:
    env.code.s
      .to(VectorMap)
      .map:
        case (k, v) => (k.sSTR: ANY, v)
      .pipe(MAP(_))

  def importQ: ENV = env.mod1(_.toFile.pipe(env.fImport).getStack(0))

  def getType: ENV = env.mod1(_.getType.sSTR)
  def envSEQ: ENV  = env.mod1(_.toSEQ)
  def envARR: ENV  = env.mod1(_.toARR)
  def envMAP: ENV  = env.mod1(_.toMAP)
  def envSTR: ENV  = env.mod1(_.toSTR)
  def vSTR: ENV    = env.vec1(_.toSTR)
  def envNUM: ENV  = env.mod1(_.toNUM)
  def vNUM: ENV    = env.vec1(_.toNUM)
  def envDBL: ENV  = env.mod1(_.toDBL)
  def vDBL: ENV    = env.vec1(_.toDBL)
  def envFN: ENV   = env.mod1(_.toFN(env))
  def envTASK: ENV = env.mod1(_.toTASK)
  def envTRY: ENV  = env.mod1(_.toTRY)
  def envYES: ENV  = env.mod1(Success(_).toTRY)
  def envNO: ENV   = env.mod1(_.toERR.x.pipe(Failure(_)).toTRY)
  def envERR: ENV  = env.mod2((x, y) => ERR(LinERR(env, y.toString, x.toString)))
  def envTF: ENV   = env.mod1(_.toTF)
  def otoTF: ENV   = env.mod1(_.toOBS.x.nonEmptyL.map(_.boolTF).toTASK)
  def envFUT: ENV  = env.mod1(_.toFUT)
  def envOBS: ENV  = env.mod1(_.toOBS)
  def toNUMD: ENV =
    env.mod2((x, y) => y.vec1(_.toInt.pipe(x.toNUM.x.getString).sSTR))
  def matchType: ENV = env.mod2(_.matchType(_))
  def lineMAP: ENV = env.vec1:
    _.toString
      .split("\n")
      .map(s => env.evalA2(Vector(), STR(s)))
      .to(VectorMap)
      .toMAP
  def fromJSON: ENV = env.vec1(_.toString.pipe(read(_)))
  def toJSON: ENV   = env.mod1(write(_).sSTR)

  def locId: ENV = env.arg1: (x, env) =>
    x.vef1(env): (env, cs) =>
      cs.xFN.foldLeft(env)((env, c) => env.addLocId(c.toString))
  def globId: ENV = env.arg1: (x, env) =>
    x.vef1(env): (env, cs) =>
      cs.xFN.foldLeft(env)((env, c) => env.addGlobId(c.toString))
  def locIdS: ENV = env.arg1: (x, env) =>
    val x1   = x.toString
    val env1 = env.addLocId(x1)
    env1.push(env1.getLineF(env1.ids(x1).l))
  def globIdS: ENV = env.arg1: (x, env) =>
    val x1   = x.toString
    val env1 = env.addGlobId(x1)
    env1.push(env1.getLineF(env1.gids(x1).l))
  def locIdF: ENV = env.arg1: (x, env) =>
    val x1   = x.toString
    val env1 = env.addLocId(x1)
    val l    = env1.ids(x1).l
    env.fnLine(l)
    env1.push(env1.getLineF(l))
  def globIdF: ENV = env.arg1: (x, env) =>
    val x1   = x.toString
    val env1 = env.addGlobId(x1)
    val l    = env1.gids(x1).l
    env.fnLine(l)
    env1.push(env1.getLineF(l))

  def lambda: ENV = env.arg1: (x, env) =>
    val x1 = x.xFN
    env.arg(x1.length): (cs, env) =>
      x1.lazyZip(cs)
        .foldLeft(env):
          case (env, (k, v)) => env.addLoc(k.toString, v)

  def clrscope: ENV = env.modScope(_ => HashMap())
  def setscope: ENV = env.arg1: (x, env) =>
    env.modScope(_ ++ x.toMAP.x.map:
      case (k, v) => (k.toString, v)
    )

  def getSc: ENV = env.push:
    MAP:
      env.code.s
        .map:
          case (k, v) => (k.sSTR, v)
        .to(VectorMap)

  def dup: ENV  = env.mods1(x => Vector(x, x))
  def dups: ENV = env.push(env.stack.toARR)
  def dupd: ENV = env.mods2((x, y) => Vector(x, x, y))
  def over: ENV = env.mods2((x, y) => Vector(x, y, x))
  def ddup: ENV = env.mods2((x, y) => Vector(x, y, x, y))
  def edup: ENV = env.mods3((x, y, z) => Vector(x, y, z, x, y, z))
  def pick: ENV =
    env.arg1((x, env) => env.push(x.vec1(n => env.getStack(n.toInt))))

  def pop: ENV = env.mods1(_ => Vector())
  def clr: ENV = env.modStack(_ => Vector())
  def nip: ENV = env.mod2((_, x) => x)
  def nix: ENV = env.arg1: (x, env) =>
    env.modStack: s =>
      val i = env.iStack(x.toInt)
      if 0 <= i && i < s.length then s.patch(i, Nil, 1) else s

  def swap: ENV  = env.mods2((x, y) => Vector(y, x))
  def rev: ENV   = env.modStack(_.reverse)
  def swapd: ENV = env.mods3((x, y, z) => Vector(y, x, z))
  def tuck: ENV  = env.mods2((x, y) => Vector(y, x, y))
  def trade: ENV =
    env.arg1((x, env) => env.push(x).rollu.push(x).push(NUM(1)).sub.roll)

  def rot: ENV  = env.mods3((x, y, z) => Vector(y, z, x))
  def rotu: ENV = env.mods3((x, y, z) => Vector(z, x, y))
  def roll: ENV = env.arg1: (x, env) =>
    val a = env.getStack(x.toInt)
    env.push(x).nix.push(a)
  def rollu: ENV = env.arg1: (x, env) =>
    val a = env.getStack(0)
    env.modStack(s => s.patch(env.iStack(x.toInt), Vector(a), 0)).pop

  def dip: ENV = env.arg2((x, f, env) => env.push(f).evale.push(x))

  def dot: ENV = env.code.x match
    case c #:: cs =>
      env
        .modCode(_ => cs)
        .pipe: env =>
          c match
            case STR(x)   => env.push(STR(StringContext.processEscapes(x)))
            case CMD(".") => env.end.evalLNext
            case CMD(x)   => env.wrapFN.push(CMD(x)).snoc
            case _        => env.push(c)
    case _ => evalLNext
