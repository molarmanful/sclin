package sclin

import better.files.*
import cats.effect.ExitCase
import monix.eval.Task
import monix.reactive.Observable
import monix.reactive.OverflowStrategy
import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.immutable.VectorMap
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.chaining.*
import scala.util.Try
import spire.implicits.*
import spire.math.*
import upickle.default.*
import ANY.*
import Lambda.*

extension (env: ENV)

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
      if env.scope.contains(x) then env.push(env.scope(x)).eval
      else if env.ids.contains(x) then env.push(NUM(env.ids(x).l)).evalLine
      else if env.gscope.contains(x) then env.push(env.gscope(x)).eval
      else if env.gids.contains(x) then env.push(NUM(env.gids(x).l)).evalLine
      else env.cmd1(x)

  def eval: ENV = env.arg1: (x, env) =>
    x match
      case f: FN =>
        val env1 =
          env.copy(code = f)
        env.code.x match
          case LazyList() => env1
          case _          => env.modStack(_ => env1.addCall().exec.stack)
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
      case cs :+ c => (cs, c.toString)
      case _       => (l.ys, ")")
    env.modCode(_ => l.xs).push(FN(env.code.p, cs)).cmd(c)

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

  def startARR: ENV = env.setArr(env.stack :: env.arr).clr
  def endARR: ENV = env.arr match
    case List()  => env
    case x :: xs => env.setArr(xs).modStack(_ => x).push(env.stack.toARR)
  def endMAP: ENV = endARR.envMAP

  def getscope: ENV = env.push:
    env.scope
      .to(VectorMap)
      .map:
        case (k, v) => (k.sSTR: ANY, v)
      .pipe(MAP(_))

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

  def clrscope: ENV = env.copy(scope = HashMap())
  def setscope: ENV = env.arg1: (x, env) =>
    env.copy(scope = env.scope ++ x.toMAP.x.map:
      case (k, v) => (k.toString, v)
    )

  def getSc: ENV = env.push:
    MAP:
      env.scope
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

  def get: ENV    = env.mod2((x, y) => y.vec1(x.get))
  def get$$ : ENV = env.mod2(_.get(_))
  def gets: ENV   = env.mod2(_.gets(_))
  def getn: ENV   = env.mod2(_.getn(_))

  def set: ENV  = env.mod3((x, y, i) => x.set(i, y))
  def sets: ENV = env.mod2((x, y) => x.sets(y.toMAP.x))
  def setn: ENV = env.mod3((x, y, i) => x.setn(i.toSEQ.x, y))

  def setmod: ENV =
    env.mod3((x, f, i) => x.setmod(i, a => SIG_1f1(f)(a)))
  def setmods: ENV = env.mod2: (x, y) =>
    x.setmods:
      y.toMAP.x.map:
        case (k, v) => (k, SIG_1f1(_: ANY)(v))
  def setmodn: ENV = env.mod3((x, f, i) => x.setmodn(i.toSEQ.x, SIG_1f1(f)))

  def idel: ENV = env.mod2(_.remove(_))

  def has: ENV    = env.mod2((x, y) => y.vec1(x.has(_).boolTF))
  def has$$ : ENV = env.mod2(_.has(_).boolTF)

  def len: ENV     = env.mod1(_.length.pipe(NUM(_)))
  def olen: ENV    = env.mod1(_.olength.map(NUM(_)).toTASK)
  def shape: ENV   = env.mod1(_.shape.map(NUM(_)).toARR)
  def dshape: ENV  = env.mod1(_.dshape.map(NUM(_)).toARR)
  def reshape: ENV = env.mod2(_ reshape _.toARR.x.map(_.toInt))
  def rank: ENV    = env.mod1(_.rank.pipe(NUM(_)))
  def depth: ENV   = env.mod1(_.depth.pipe(NUM(_)))

  def rep: ENV  = env.mod1(LazyList.continually(_).toSEQ)
  def orep: ENV = env.mod1(Observable.repeat(_).toOBS)
  def cyc: ENV  = env.mod1(x => LazyList.continually(x).mSEQ(x).flat)
  def ocyc: ENV = env.mod1(Observable.repeat(_).toOBS.flat)
  def ones: ENV = env.vec1(_.toInt.pipe(Vector.fill(_)(NUM(1)).toARR))
  def one$ : ENV =
    env.mod1(_.foldRight(NUM(1))((x, y) => Vector.fill(x.toInt)(y).toARR))
  def toShape: ENV =
    env.mod2(_.toShape(_))

  def itr: ENV = env.mod2: (x, y) =>
    y.vec1(f => LazyList.iterate(x)(SIG_1f1(f)(_)).toSEQ)
  def oitr: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      Observable
        .unfoldEval(x)(s => SIG_1f1(f)(s).toTASK.x.map(Some(s, _)))
        .toOBS
  def unfold: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      LazyList
        .unfold(x): s =>
          env.evalS(Vector(s), f) match
            case Vector()    => None
            case _ :+ m :+ n => Some(m, n)
            case _           => throw LinEx("ST_LEN", "stack length = 1")
        .toSEQ
  def ounfold: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      Observable
        .unfoldEval(x):
          SIG_1f1(f)(_).toTASK.x.map: a =>
            a.toARR.x match
              case Vector()    => None
              case _ :+ m :+ n => Some(m, n)
              case _           => throw LinEx("ST_LEN", "stack length = 1")
        .toOBS

  def enumL: ENV = env.mod1:
    case x: MAP => x.toARR
    case OBS(x) =>
      Observable
        .fromIterable(ANY.wholes)
        .zipMap(x)(Vector(_, _).toARR)
        .toOBS
    case x =>
      ANY.wholes.toSEQ
        .zip(x)(Vector(_, _).toARR)
        .matchType(x)
  def keys: ENV = env.enumL.mod1(_.map(_.get(NUM(0))))
  def vals: ENV = env.enumL.mod1(_.map(_.get(NUM(1))))

  def rhelper(r: (Real, Real) => Range): ENV =
    env.num2a(r(_, _).iterator.map(Real(_)).toVector)
  def range: ENV =
    rhelper((x, y) => x.intValue until y.intValue by (y > x).boolInt * 2 - 1)
  def irange: ENV =
    rhelper((x, y) => x.intValue to y.intValue by (y > x).boolInt * 2 - 1)

  def shuffle: ENV = env.mod1(_.shuffle)
  def getr: ENV    = env.shuffle.push(NUM(0)).get
  def perm: ENV    = env.mod1(_.permutations)
  def comb: ENV    = env.mod2((x, y) => y.vec1(n => x.combinations(n.toInt)))
  def powset: ENV  = env.mod1(_.powset)
  def cProd: ENV = env.mod1: x =>
    x.toSEQ.x
      .map(_.toSEQ.x)
      .pipe(Util.cProd)
      .map(_.toSEQ.mIts(x.get(NUM(0))))
      .toSEQ
  def transpose: ENV = env.mod1: x =>
    def f(a: ANY): LazyList[ANY] = a match
      case MAP(a) => a.values.to(LazyList)
      case _      => a.toSEQ.x
    def g(a: LazyList[ANY], b: ANY): ANY = b match
      case Lsy(_) => a.mSEQ(b)
      case _: MAP => a.toARR
      case Its(_) => a.toARR.matchType(b)
      case _      => a.toARR
    val x1 = f(x)
    x1.map(f)
      .pipe(Util.transpose)
      .map(g(_, x))
      .pipe(g(_, x1.headOption.getOrElse(UN.toARR)))
  def raxes: ENV = env.mod1(_.raxes)
  def paxes: ENV = env.mod2(_ paxes _.toARR.x.map(_.toInt))

  def padH(f: (String, Int, String) => String): ENV = env.vec3: (x, y, z) =>
    val x1 = x.toString
    val y1 = y.toInt - x1.length
    val z1 = z.toString
    f(x1, y1, z1).sSTR
  def padH1(s: String, n: Int): String =
    LazyList.continually(s).flatten.take(n).mkString
  def pad: ENV  = padH((x, y, z) => x + padH1(z, y))
  def padl: ENV = padH((x, y, z) => padH1(z, y) + x)
  def padc: ENV = padH: (x, y, z) =>
    val q  = y / 2
    val y1 = y - q
    padH1(z, q) + x + padH1(z, y1)

  def pad$H(f: (ANY, Int, ANY) => ANY): ENV = env.mod3: (x, y, z) =>
    y.vec1: y =>
      val y1 = y.toInt - x.length
      if x.toSEQ.x.drop(y1).isEmpty then f(x, y1, z) else x
  def pad$H1(s: ANY, n: Int): ANY = ARR(Vector()).add$$(s).mul$$(NUM(n))
  def pad$ : ENV                  = pad$H((x, y, z) => x.add$$(pad$H1(z, y)))
  def padl$ : ENV                 = pad$H((x, y, z) => pad$H1(z, y).add$$(x))
  def padc$ : ENV = pad$H: (x, y, z) =>
    val q  = y / 2
    val y1 = y - q
    pad$H1(z, q).add$$(x).add$$(pad$H1(z, y1))

  def toCodePt: ENV =
    env.mod1(_.vec1(x => x.toString.map(_.toInt.pipe(NUM(_))).toARR))
  def fromCodePt: ENV = env.mod1:
    _.map(_.toInt.toChar.toString.sSTR).toString.sSTR

  def split: ENV   = env.str2a(_.split(_))
  def ssplit: ENV  = env.str1a(_.split(raw"\s"))
  def join: ENV    = env.mod2((x, y) => y.str1(x.join))
  def toUpper: ENV = env.str1(_.toUpperCase)
  def toLower: ENV = env.str1(_.toLowerCase)
  def toCap: ENV   = env.str1(_.capitalize)

  def rmatchBase(x: ANY, r: ANY): Iterator[MAP] =
    r.toString.r.findAllMatchIn(x.toString).map(_.matchMAP)
  def rmatch: ENV       = env.vec2(rmatchBase(_, _).toSEQ)
  def rmatchMatch: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("&"))).toSEQ)
  def rmatchBefore: ENV = env.vec2(rmatchBase(_, _).map(_.get(STR("`"))).toSEQ)
  def rmatchAfter: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("'"))).toSEQ)
  def rmatchGroups: ENV = env.vec2(rmatchBase(_, _).map(_.get(STR("*"))).toSEQ)
  def rmatchStart: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("^"))).toSEQ)
  def rmatchEnd: ENV    = env.vec2(rmatchBase(_, _).map(_.get(STR("$"))).toSEQ)
  def rsub: ENV = env.vec3: (x, r, f) =>
    r.toString.r
      .replaceAllIn(x.toString, m => env.evalA1(Vector(m.matchMAP), f).toString)
      .sSTR
  def rsubFirst: ENV =
    env.vec3((x, r, f) =>
      r.toString.r.replaceFirstIn(x.toString, f.toString).sSTR
    )

  def wrap$ : ENV   = env.modx(2)(_.toARR)
  def wrap: ENV     = env.modx(1)(_.toARR)
  def wrap$$ : ENV  = env.modStack(x => Vector(x.toARR))
  def wrapv$ : ENV  = env.vec2(Vector(_, _).toARR)
  def wrapv: ENV    = env.vec1(Vector(_).toARR)
  def unwrap: ENV   = env.mods1(_.toARR.x)
  def unwrap$ : ENV = env.arg1((x, env) => env.modStack(_ => x.toARR.x))
  def wrapFN: ENV   = env.wrap.mod1(_.toFN(env))

  def tk: ENV = env.mod2((x, y) => y.vec1(n => x.take(n.toInt)))
  def otk: ENV =
    env.mod2((x, y) => y.vec1(n => x.modOBS(_.takeByTimespan(n.toMs))))
  def otko: ENV = env.mod2((x, y) => x.modOBS(_.takeUntil(y.toOBS.x)))
  def dp: ENV   = env.mod2((x, y) => y.vec1(n => x.drop(n.toInt)))
  def odp: ENV =
    env.mod2((x, y) => y.vec1(n => x.modOBS(_.dropByTimespan(n.toMs))))
  def odpo: ENV = env.mod2((x, y) => x.modOBS(_.dropUntil(y.toOBS.x)))
  def splitAt: ENV = env.mod2: (x, y) =>
    y.vec1: n =>
      val i = n.toInt
      Vector(x.take(i), x.drop(i)).toARR

  def scale: ENV = env.push(NUM(10)).swap.pow.mul
  def trunc: ENV = env.num1(_.toBigInt.toDouble, _.toBigInt)
  def floor: ENV = env.num1(_.floor, _.floor)
  def round: ENV = env.num1(_.round, _.round)
  def ceil: ENV  = env.num1(_.ceil, _.ceil)

  def fromDec: ENV =
    env.num2a((x, y) => Util.fromDec(x.toSafeLong, y.toInt).map(Real(_)))
  def toDec: ENV = env.mod2: (x, y) =>
    val x1 = x.toARR.x.map(_.toNUM.x.toSafeLong)
    y.num1(n => Util.toDec(x1, n.toInt).toDouble, n => Util.toDec(x1, n.toInt))
  def toNumDen: ENV = env.vec1: x =>
    val a = x.toNUM.x.toRational
    Vector(a.numerator, a.denominator).map(NUM(_)).toARR
  def isInt: ENV = env.vec1(_.toNUM.x.isWhole.boolTF)
  def isExact: ENV = env.vec1: x =>
    TF:
      x.toNUM.x match
        case Real.Exact(_) => true
        case _             => false

  def neg: ENV   = env.num1(-_, -_)
  def neg$ : ENV = env.str1(_.reverse)
  def neg$$ : ENV =
    def loop(t: ANY): ANY = t match
      case Lsy(x) => x.reverse.mSEQ(t)
      case ARR(x) => x.reverse.toARR
      case _: MAP => loop(t.toSEQ).toMAP
      case _      => t.str1(_.reverse)
    env.mod1(loop)

  def add: ENV    = env.num2(_ + _, _ + _)
  def add$ : ENV  = env.str2(_ ++ _)
  def add$$ : ENV = env.mod2(_ add$$ _)
  def cons: ENV   = env.mod2(_ cons _)
  def snoc: ENV   = env.mod2(_ snoc _)
  def uncons: ENV = env.mods1(x => Vector(x.get(NUM(0)), x.drop(1)))
  def unsnoc: ENV = env.mods1(x => Vector(x.drop(-1), x.get(NUM(-1))))

  def sub: ENV    = env.num2(_ - _, _ - _)
  def sub$ : ENV  = env.str2((x, y) => x.filterNot(y.contains))
  def sub$$ : ENV = env.mod2(_ sub$$ _)

  def mul: ENV    = env.num2(_ * _, _ * _)
  def mul$ : ENV  = env.strnum(_ * _.intValue)
  def mul$$ : ENV = env.mod2(_ mul$$ _)

  def div: ENV = env.num2(
    _ / _,
    (x, y) => if y == 0 then throw ArithmeticException() else x / y,
    "bad /"
  )
  def divi: ENV = env.num2(
    _.fquot(_),
    (x, y) => if y == 0 then throw ArithmeticException() else x.fquot(y),
    "bad /~"
  )
  def div$ : ENV  = env.strnumq((x, y) => x.grouped(y.intValue).to(LazyList))
  def div$$ : ENV = env.mod2((x, y) => y.vec1(x div$$ _.toInt))

  def mod: ENV = env.num2(
    _.fmod(_),
    (x, y) => if y == 0 then throw ArithmeticException() else x.fmod(y),
    "bad %"
  )
  def divmod: ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).divi.pushs(Vector(x, y)).mod
  def mod$ : ENV  = env.strnumq((x, y) => x.sliding(y.intValue).to(LazyList))
  def mod$$ : ENV = env.mod2((x, y) => y.vec1(x mod$$ _.toInt))
  def mod2$$ : ENV =
    env.mod3((x, y, z) => y.vec2(z)((n, k) => x.mod$$(n.toInt, k.toInt)))

  def pow: ENV = env.num2(
    _.fpow(_),
    (x, y) =>
      if x < 0 && y.reciprocal.fmod(2) == 0 then throw ArithmeticException()
      else x.fpow(y),
    "bad ^"
  )
  def powi: ENV = env.num2(_ ** _.intValue, _ ** _.intValue)
  def pow$ : ENV = env.vec2: (x, y) =>
    Util.cPow(x.toSTR.toSEQ.x, y.toInt).map(_.toARR.toSTR).toSEQ
  def pow$$ : ENV = env.mod2: (x, y) =>
    y.vec1(n => Util.cPow(x.toSEQ.x, n.toInt).map(_.toARR.matchType(x)).toSEQ)

  def exp: ENV = env.num1(_.exp, _.exp)
  def abs: ENV = env.num1(_.abs, _.abs)

  def sin: ENV = env.num1(math.sin, Real.sin)
  def cos: ENV = env.num1(math.cos, Real.cos)
  def tan: ENV = env.num1(
    math.tan,
    x =>
      val d = Real.cos(x)
      if d == 0 then throw ArithmeticException() else Real.sin(x) / d
    ,
    "bad tan"
  )
  def asin: ENV  = env.num1(math.asin, Real.asin)
  def acos: ENV  = env.num1(math.acos, Real.acos)
  def atan: ENV  = env.num1(math.atan, Real.atan)
  def atan2: ENV = env.num2(math.atan2, Real.atan2)
  def sinh: ENV  = env.num1(math.sinh, Real.sinh)
  def cosh: ENV  = env.num1(math.cosh, Real.cosh)
  def tanh: ENV  = env.num1(math.tanh, Real.tanh)
  def asinh: ENV = env.num1(Util.asinh, Real.asinh)
  def acosh: ENV = env.num1(Util.acosh, Real.acosh)
  def atanh: ENV = env.num1(Util.atanh, Real.atanh)

  def ln: ENV =
    env.num1(
      math.log,
      x => if x <= 0 then throw ArithmeticException() else Real.log(x),
      "bad log"
    )
  def log: ENV   = env.arg2((x, y, env) => env.push(x).ln.push(y).ln.div)
  def log10: ENV = env.push(NUM(10)).log

  def isPrime: ENV = env.vec1(_.toNUM.x.toSafeLong.pipe(prime.isPrime).boolTF)
  def factor: ENV = env.vec1:
    _.toNUM.x.toSafeLong
      .pipe(prime.factor)
      .to(VectorMap)
      .map:
        case (x, y) => (NUM(x), NUM(y))
      .toMAP
      .sortBy((a, _) => a, a => a)

  def not: ENV    = env.vec1(_.toBool.pipe(!_).boolTF)
  def not$$ : ENV = env.mod1(_.toBool.pipe(!_).boolTF)
  def min: ENV    = env.vec2((x, y) => if x.cmp(y) < 0 then x else y)
  def and: ENV    = env.vec2((x, y) => (x.toBool && y.toBool).boolTF)
  def min$$ : ENV = env.mod2((x, y) => if x.cmp(y) < 0 then x else y)
  def and$$ : ENV = env.mod2((x, y) => (x.toBool && y.toBool).boolTF)
  def max: ENV    = env.vec2((x, y) => if x.cmp(y) > 0 then x else y)
  def or: ENV     = env.vec2((x, y) => (x.toBool || y.toBool).boolTF)
  def max$$ : ENV = env.mod2((x, y) => if x.cmp(y) > 0 then x else y)
  def or$$ : ENV  = env.mod2((x, y) => (x.toBool || y.toBool).boolTF)

  def cmp: ENV    = env.vec2((x, y) => NUM(x.cmp(y)))
  def cmp$$ : ENV = env.mod2((x, y) => NUM(x.cmp(y)))
  def lt: ENV     = cmp.push(NUM(-1)).eql
  def lt$$ : ENV  = cmp$$.push(NUM(-1)).eql
  def gt: ENV     = cmp.push(NUM(1)).eql
  def gt$$ : ENV  = cmp$$.push(NUM(1)).eql
  def lteq: ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).lt.pushs(Vector(x, y)).eql.or
  def lteq$$ : ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).lt$$.pushs(Vector(x, y)).eql$$.or
  def gteq: ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).gt.pushs(Vector(x, y)).eql.or
  def gteq$$ : ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).gt$$.pushs(Vector(x, y)).eql$$.or
  def eql: ENV     = env.vec2(_.eql(_).boolTF)
  def eql$$ : ENV  = env.mod2(_.eql(_).boolTF)
  def eqls: ENV    = env.vec2(_.eqls(_).boolTF)
  def eqls$$ : ENV = env.mod2(_.eqls(_).boolTF)
  def neq: ENV     = eql.not
  def neq$$ : ENV  = eql$$.not
  def neqs: ENV    = eqls.not
  def neqs$$ : ENV = eqls$$.not

  def bhelp1(f: Long => Long, g: SafeLong => NUMF): ENV =
    env.num1(_.toLong.pipe(f).toDouble, _.toSafeLong.pipe(g))
  def bhelp2(f: (Long, Long) => Long, g: (SafeLong, SafeLong) => NUMF): ENV =
    env.num2(
      (a, b) => f(a.toLong, b.toLong).toDouble,
      (a, b) => g(a.toSafeLong, b.toSafeLong)
    )
  def bNOT: ENV  = bhelp1(~_, ~_.toBigInt)
  def bAND: ENV  = bhelp2(_ & _, _ & _)
  def bOR: ENV   = bhelp2(_ | _, _ | _)
  def bXOR: ENV  = bhelp2(_ ^ _, _ ^ _)
  def bLSH: ENV  = bhelp2(_ << _, _ << _.toInt)
  def bRSH: ENV  = bhelp2(_ >> _, _ >> _.toInt)
  def bURSH: ENV = bhelp2(_ >>> _, _.toLong >>> _.toLong)

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

  def map: ENV =
    env.mod2((x, y) => y.vec1(f => x.map(SIG_2f2(f), SIG_1f1(f))))
  def mapEval: ENV =
    env.mod2((x, y) => y.vec1(f => x.mapEval(SIG_1f1(f)).toOBS))
  def mapPar: ENV = env.mod3: (x, y, z) =>
    y.vec2(z): (f, n) =>
      x.modOBS(_.mapParallelUnordered(n.toInt)(SIG_1f1(f)(_).toTASK.x))
  def mapParOrd: ENV = env.mod3: (x, y, z) =>
    y.vec2(z): (f, n) =>
      x.modOBS(_.mapParallelOrdered(n.toInt)(SIG_1f1(f)(_).toTASK.x))
  def tapMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.map(SIG_2f_(f), SIG_1f_(f))))
  def flatMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.flatMap(SIG_2f1(f), SIG_1f1(f))))
  def mergeMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.mergeMap(SIG_1f1(f)).toOBS))
  def switchMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.switchMap(SIG_1f1(f)).toOBS))
  def winMap: ENV = env.mod3: (x, y, z) =>
    y.vec2(z)((f, n) => x.winMapM(n.toInt, env.evalA2(_, f), env.evalA1(_, f)))
  def flat: ENV  = env.mod1(_.flat)
  def merge: ENV = env.mod1(_.merge.toOBS)
  def rflat: ENV = env.mod1(_.rflat)
  def rmap: ENV  = env.mod2((x, y) => y.vec1(f => x.rmap(SIG_1f1(f))))
  def dmap: ENV =
    env.mod3((x, y, z) => y.vec2(z)((f, d) => x.dmap(d.toInt, SIG_1f1(f))))

  def foreach: ENV =
    env.mod2((x, y) => y.vec1(f => x.foreach(SIG_2f_(f), SIG_1f_(f))))

  def zip: ENV = env.mod3((x, y, z) => z.vec1(f => x.zip(y)(SIG_2f1(f))))
  def zip$ : ENV = env.modx(5):
    case Vector(x, y, v, w, z) => z.vec1(f => x.zipAll(y, v, w, SIG_2f1(f)))
  def tbl: ENV  = env.mod3((x, y, z) => z.vec1(f => x.table(y, SIG_2f1(f))))
  def tblf: ENV = env.mod3((x, y, z) => z.vec1(f => x.flatTable(y, SIG_2f1(f))))

  def fold: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.foldLeft(y)(SIG_1y2f1(f), SIG_2f1(f))))
  def ofold: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.ofoldLeft(y)(SIG_2f1(f)).toTASK))
  def foldR: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.foldRight(y)(SIG_2y1f1(f), SIG_2f1(f))))
  def rfold: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.rfoldLeft(y)(SIG_2f1(f))))
  def rfoldR: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.rfoldRight(y)(SIG_2f1(f))))
  def reduce: ENV =
    env.mod2((x, y) => y.vec1(f => x.reduceLeft(SIG_1y2f1(f), SIG_2f1(f))))
  def reduceR: ENV =
    env.mod2((x, y) => y.vec1(f => x.reduceRight(SIG_1y2f1(f), SIG_2f1(f))))
  def scan: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.scanLeft(y)(SIG_1y2f1(f), SIG_2f1(f))))
  def scanEval: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.scanEval(y)(SIG_2f1(f)).toOBS))
  def scanR: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.scanRight(y)(SIG_2y1f1(f), SIG_2f1(f))))

  def walk: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      def loop(b: Boolean = false)(xs: ARRW[ANY]): ARRW[ANY] = xs match
        case st :+ n =>
          st match
            case st :+ m if b =>
              st match
                case _ :+ k => Vector(k, if n.toBool then fn(m) else m)
                case _ =>
                  val n1 = n match
                    case Itr(_) => fn(n)
                    case _      => n
                  Vector(m, n1)
            case _ :+ m => Vector(if n.toBool then fn(m) else m)
            case _ =>
              Vector:
                n match
                  case Itr(_) => fn(n)
                  case _      => n
        case _ => Vector()
      def fn(t: ANY): ANY =
        t.flatMap$(
          (k, v) => env.evalS(Vector(k, v), f).pipe(loop(true)),
          v => env.evalS(Vector(v), f).pipe(loop())
        )
      env.evalS(Vector(x), f).pipe(loop()).lastOption.getOrElse(UN)

  def fltr: ENV =
    env.mod2((x, y) => y.vec1(f => x.filter(SIG_2fb(f), SIG_1fb(f))))
  def ofltr: ENV =
    env.mod2((x, y) => y.vec1(f => x.filterEval(SIG_1f1(f)).toOBS))

  def any: ENV =
    env.mod2((x, y) => y.vec1(f => x.any(SIG_2fb(f), SIG_1fb(f)).boolTF))
  def oany: ENV =
    env.mod2((x, y) => y.vec1(f => x.oany(SIG_1fb(f)).map(_.boolTF).toTASK))
  def all: ENV =
    env.mod2((x, y) => y.vec1(f => x.all(SIG_2fb(f), SIG_1fb(f)).boolTF))
  def oall: ENV =
    env.mod2((x, y) => y.vec1(f => x.oall(SIG_1fb(f)).map(_.boolTF).toTASK))

  def tkwl: ENV =
    env.mod2((x, y) => y.vec1(f => x.takeWhile(SIG_2fb(f), SIG_1fb(f))))
  def dpwl: ENV =
    env.mod2((x, y) => y.vec1(f => x.dropWhile(SIG_2fb(f), SIG_1fb(f))))

  def find: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      x.find(SIG_2fb(f), SIG_1fb(f)) match
        case Some((a, b)) => Vector(a, b).toARR
        case Some(a: ANY) => a
        case None         => UN
  def ofind: ENV = env.mod2: (x, y) =>
    y.vec1(f => x.ofind(SIG_1fb(f)).map(_.getOrElse(UN)).toTASK)

  def findi: ENV =
    env.mod2((x, y) => y.vec1(f => x.findIndex(SIG_1fb(f)).pipe(NUM(_))))

  def del: ENV =
    env.mod2((x, y) => y.vec1(f => x.delBy(SIG_2fb(f), SIG_1fb(f))))

  def uniq: ENV =
    env.mod2((x, y) => y.vec1(f => x.uniqBy(SIG_2f1(f), SIG_1f1(f))))
  def uniq$ : ENV =
    env.mod2((x, y) => y.vec1(f => x.uniqWith(SIG_2x2fb(f), SIG_2fb(f))))

  def sort: ENV =
    env.mod2((x, y) => y.vec1(f => x.sortBy(SIG_2f1(f), SIG_1f1(f))))
  def sort$ : ENV =
    env.mod2((x, y) => y.vec1(f => x.sortWith(SIG_2x2fb(f), SIG_2fb(f))))

  def part: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      val (a, b) = x.partition(SIG_2fb(f), SIG_1fb(f))
      Vector(a, b).toARR

  def group: ENV =
    env.mod2((x, y) => y.vec1(f => x.groupBy(SIG_2f1(f), SIG_1f1(f)).toMAP))
  def ogroup: ENV =
    env.mod2((x, y) => y.vec1(f => x.ogroupBy(SIG_1f1(f)).toOBS))

  def span: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      val (a, b) = x.span(SIG_2fb(f), SIG_1fb(f))
      Vector(a, b).toARR

  def pack: ENV =
    env.mod2((x, y) => y.vec1(f => x.packWith(SIG_2x2fb(f), SIG_2fb(f))))

  def union: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.unionWith(y, SIG_2fb(f))))
  def intersect: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.intersectWith(y, SIG_2fb(f))))
  def diff: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.differWith(y, SIG_2fb(f))))

  def evalTASK: ENV = env.arg1: (x, env) =>
    env.push(x.vec1(f => Task(env.push(f).quar.getStack(0)).toTASK))
  def await: ENV =
    env.vec1(x => Await.result(x.toFUT.x, Duration.Inf))
  def cancelFUT: ENV = env.arg1: (x, env) =>
    x.vec1(_.toFUT.x.cancel().pipe(_ => UN))
    env
  def asyncBound: ENV = env.mod1:
    case x: OBS => x.modOBS(_.asyncBoundary(OverflowStrategy.Unbounded))
    case x      => x.vec1(_.modTASK(_.asyncBoundary))
  def forkTASK: ENV   = env.vec1(_.modTASK(_.executeAsync))
  def forkIOTASK: ENV = env.vec1(_.modTASK(_.executeOn(env.ioSched)))
  def memoTASK: ENV = env.mod1:
    case OBS(x) => x.cache.toOBS
    case x      => x.vec1(_.modTASK(_.memoize))
  def memoTASK$ : ENV = env.vec1(_.modTASK(_.memoizeOnSuccess))
  def uncancelTASK: ENV = env.mod1:
    case OBS(x) => x.uncancelable.toOBS
    case x      => x.vec1(_.modTASK(_.uncancelable))
  def timeoutTASK: ENV = env.vec2: (x, n) =>
    val n1 = n.toNUM.x.toLong
    x.modTASK:
      _.timeoutWith(n1.milliseconds, LinEx("TASK", s"timeout after ${n1}ms"))
  def itrTASKW(t: ANY, f: Iterable[Task[ANY]] => Task[Iterable[ANY]]): ANY =
    t match
      case MAP(x) =>
        x.values
          .map(_.toTASK.x)
          .pipe(f)
          .map:
            _.zip(x.keys)
              .map:
                case (v, k) => (k, v)
              .to(VectorMap)
              .toMAP
          .toTASK
      case Itr(x) =>
        x.toSEQ.x
          .map(_.toTASK.x)
          .pipe(f)
          .map(_.mSEQ(x))
          .toTASK
      case x => x.toTASK
  def seqTASK: ENV = env.mod1(itrTASKW(_, Task.sequence))
  def parTASK: ENV = env.mod1(itrTASKW(_, Task.parSequence))
  def parnTASK: ENV =
    env.mod2((x, n) => itrTASKW(x, Task.parSequenceN(n.toInt)))
  def parunTASK: ENV = env.mod1(itrTASKW(_, Task.parSequenceUnordered))
  def raceTASK: ENV =
    env.mod1(_.toSEQ.x.map(_.toTASK.x).pipe(Task.raceMany).toTASK)
  def timeTASK: ENV = env.vec1:
    _.modTASK:
      _.timed.map:
        case (t, a) => Vector(t.toMillis.toNUM, a).toARR
  def redeemTASK: ENV = env.vec3: (x, f, g) =>
    x.modTASK:
      _.redeemWith(e => SIG_1f1(f)(ERR(e)).toTASK.x, SIG_1f1(g)(_).toTASK.x)
  def restartwTASK: ENV = env.mod2: (x, y) =>
    x match
      case _: OBS => y.vec1(f => x.modTASK(_.restartUntil(SIG_1fb(f))))
      case _      => x.vec2(y)((t, f) => t.modTASK(_.restartUntil(SIG_1fb(f))))
  def wrapTRYTASK: ENV   = env.vec1(_.modTASK(_.materialize.map(_.toTRY)))
  def unwrapTRYTASK: ENV = env.vec1(_.modTASK(_.map(_.toTRY.x).dematerialize))
  def bracketTASK: ENV = env.mod3: (x, y, z) =>
    def go(f: ANY)(a: ANY, e: ExitCase[Throwable]) =
      SIG_2f1(f)(a, ANY.exitCase(e)).toTASK.x.map(_ => ())
    x match
      case _: OBS =>
        y.vec2(z): (f, g) =>
          x.modOBS(_.bracketCase(SIG_1f1(f)(_).toOBS.x)(go(g)))
      case _ =>
        x.vec3(y, z): (t, f, g) =>
          t.modTASK(_.bracketCase(SIG_1f1(f)(_).toTASK.x)(go(g)))
  def onErrTASK: ENV = env.mod2: (x, y) =>
    x match
      case _: OBS =>
        y.vec1: f =>
          x.modOBS(_.onErrorHandleWith(e => SIG_1f1(f)(ERR(e)).toOBS.x))
      case _ =>
        x.vec2(y): (t, f) =>
          t.modTASK(_.onErrorHandleWith(e => SIG_1f1(f)(ERR(e)).toTASK.x))
  def sleepTASK: ENV = env.vec1: n =>
    val n1 = n.toNUM.x.toLong
    n1.milliseconds.pipe(Task.sleep).map(_ => NUM(n1)).toTASK
  def delayTASK: ENV = env.mod2: (x, y) =>
    x match
      case _: OBS => y.vec1(n => x.modOBS(_.delayExecution(n.toMs)))
      case _      => x.vec2(y)((t, n) => t.modTASK(_.delayExecution(n.toMs)))

  def ostratUn: ENV = env.push(OSTRAT(OverflowStrategy.Unbounded))
  def ostratFail: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.Fail.apply).pipe(OSTRAT(_)))
  def ostratBack: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.BackPressure.apply).pipe(OSTRAT(_)))
  def ostratNew: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.DropNew.apply).pipe(OSTRAT(_)))
  def ostratOld: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.DropOld.apply).pipe(OSTRAT(_)))
  def ostratClr: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.ClearBuffer.apply).pipe(OSTRAT(_)))

  def ocache: ENV = env.mod2((x, y) => y.vec1(n => x.modOBS(_.cache(n.toInt))))
  def obufferN: ENV = env.mod2: (x, y) =>
    y.vec1(n => x.modOBS(_.bufferIntrospective(n.toInt).map(_.toARR)))
  def obufferT: ENV = env.mod2: (x, y) =>
    y.vec1(n => x.modOBS(_.bufferTimed(n.toMs).map(_.toARR)))
  def obufferTN: ENV = env.mod3: (x, y, z) =>
    y.vec2(z): (t, n) =>
      x.modOBS(_.bufferTimedAndCounted(t.toMs, n.toInt).map(_.toARR))
  def obufferTB: ENV = env.modx(4):
    case Vector(x, y, v, w) =>
      y.vec3(v, w): (t, n, f) =>
        x.modOBS:
          _.bufferTimedWithPressure(t.toMs, n.toInt, SIG_1f1(f)(_).toInt)
            .map(_.toARR)
  def obufferON: ENV = env.mod3: (x, y, z) =>
    z.vec1(n => x.modOBS(_.bufferWithSelector(y.toOBS.x, n.toInt).map(_.toARR)))
  def othrottle: ENV = env.mod3: (x, y, z) =>
    y.vec2(z)((t, n) => x.modOBS(_.throttle(t.toMs, n.toInt).map(_.toARR)))
  def othrottleFirst: ENV =
    env.mod2((x, y) => y.vec1(n => x.modOBS(_.throttleFirst(n.toMs))))
  def othrottleLast: ENV =
    env.mod2((x, y) => y.vec1(n => x.modOBS(_.throttleLast(n.toMs))))
  def odebounce: ENV =
    env.mod2((x, y) => y.vec1(n => x.modOBS(_.debounce(n.toMs))))
  def oasyncBound: ENV =
    env.mod2((x, y) => x.modOBS(_.asyncBoundary(y.toOSTRAT.x)))
  def odelay: ENV =
    env.mod2((x, y) => x.modOBS(_.delayExecutionWith(y.toOBS.x)))
  def ointervald: ENV =
    env.vec1(n => Observable.interval(n.toMs).map(NUM(_)).toOBS)
  def ointervalr: ENV =
    env.vec1(n => Observable.intervalAtFixedRate(n.toMs).map(NUM(_)).toOBS)

  def dot: ENV = env.code.x match
    case c #:: cs =>
      env
        .modCode(_ => cs)
        .pipe: env =>
          c match
            case STR(x) => env.push(STR(StringContext.processEscapes(x)))
            case CMD(x) => env.wrapFN.push(CMD(x)).snoc
            case _      => env.push(c)
    case _ => evalLNext
