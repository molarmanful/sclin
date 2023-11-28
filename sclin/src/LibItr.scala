package sclin

import better.files.*
import monix.reactive.Observable
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.chaining.*
import spire.implicits.*
import spire.math.*
import upickle.default.*
import ANY.*
import Lambda.*

extension (env: ENV)

  def get: ENV    = env.mod2((x, y) => y.vec1(x.get))
  def get$$ : ENV = env.mod2(_.get(_))
  def gets: ENV   = env.mod2(_.gets(_))
  def getn: ENV   = env.mod2(_.getn(_))

  def set: ENV  = env.mod3((x, y, i) => x.set(i, y))
  def sets: ENV = env.mod2((x, y) => x.sets(y.toMAP.x))
  def setn: ENV = env.mod3((x, y, i) => x.setn(i.toSEQ.x, y))

  def setmod: ENV =
    env.mod3((x, f, i) => x.setmod(i, a => env.SIG_1f1(f)(a)))
  def setmods: ENV = env.mod2: (x, y) =>
    x.setmods:
      y.toMAP.x.map:
        case (k, v) => (k, env.SIG_1f1(_: ANY)(v))
  def setmodn: ENV = env.mod3((x, f, i) => x.setmodn(i.toSEQ.x, env.SIG_1f1(f)))

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

  def wrap$ : ENV   = env.modx(2)(_.toARR)
  def wrap: ENV     = env.modx(1)(_.toARR)
  def wrap$$ : ENV  = env.modStack(x => Vector(x.toARR))
  def wrapv$ : ENV  = env.vec2(Vector(_, _).toARR)
  def wrapv: ENV    = env.vec1(Vector(_).toARR)
  def unwrap: ENV   = env.mods1(_.toARR.x)
  def unwrap$ : ENV = env.arg1((x, env) => env.modStack(_ => x.toARR.x))
  def wrapFN: ENV   = env.wrap.mod1(_.toFN(env))

  def consFN: ENV =
    env.mod2((x, y) => Vector(x).toARR.cons(CMD(",_").cons(y.toFN(env))))
  def concatFN: ENV = env.mod2((x, y) => x.cons(CMD(",_").cons(y.toFN(env))))

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
    y.vec1(f => LazyList.iterate(x)(env.SIG_1f1(f)(_)).toSEQ)
  def oitr: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      Observable
        .unfoldEval(x)(s => env.SIG_1f1(f)(s).toTASK.x.map(Some(s, _)))
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
          env
            .SIG_1f1(f)(_)
            .toTASK
            .x
            .map: a =>
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

  def map: ENV =
    env.mod2((x, y) => y.vec1(f => x.map(env.SIG_2f2(f), env.SIG_1f1(f))))
  def mapEval: ENV =
    env.mod2((x, y) => y.vec1(f => x.mapEval(env.SIG_1f1(f)).toOBS))
  def mapPar: ENV = env.mod3: (x, y, z) =>
    y.vec2(z): (f, n) =>
      x.modOBS(_.mapParallelUnordered(n.toInt)(env.SIG_1f1(f)(_).toTASK.x))
  def mapParOrd: ENV = env.mod3: (x, y, z) =>
    y.vec2(z): (f, n) =>
      x.modOBS(_.mapParallelOrdered(n.toInt)(env.SIG_1f1(f)(_).toTASK.x))
  def tapMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.map(env.SIG_2f_(f), env.SIG_1f_(f))))
  def flatMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.flatMap(env.SIG_2f1(f), env.SIG_1f1(f))))
  def mergeMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.mergeMap(env.SIG_1f1(f)).toOBS))
  def switchMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.switchMap(env.SIG_1f1(f)).toOBS))
  def winMap: ENV = env.mod3: (x, y, z) =>
    y.vec2(z)((f, n) => x.winMapM(n.toInt, env.evalA2(_, f), env.evalA1(_, f)))
  def flat: ENV  = env.mod1(_.flat)
  def merge: ENV = env.mod1(_.merge.toOBS)
  def rflat: ENV = env.mod1(_.rflat)
  def rmap: ENV  = env.mod2((x, y) => y.vec1(f => x.rmap(env.SIG_1f1(f))))
  def dmap: ENV =
    env.mod3((x, y, z) => y.vec2(z)((f, d) => x.dmap(d.toInt, env.SIG_1f1(f))))

  def foreach: ENV =
    env.mod2((x, y) => y.vec1(f => x.foreach(env.SIG_2f_(f), env.SIG_1f_(f))))

  def zip: ENV = env.mod3((x, y, z) => z.vec1(f => x.zip(y)(env.SIG_2f1(f))))
  def zip$ : ENV = env.modx(5):
    case Vector(x, y, v, w, z) => z.vec1(f => x.zipAll(y, v, w, env.SIG_2f1(f)))
  def tbl: ENV = env.mod3((x, y, z) => z.vec1(f => x.table(y, env.SIG_2f1(f))))
  def tblf: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.flatTable(y, env.SIG_2f1(f))))

  def fold: ENV =
    env.mod3((x, y, z) =>
      z.vec1(f => x.foldLeft(y)(env.SIG_1y2f1(f), env.SIG_2f1(f)))
    )
  def ofold: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.ofoldLeft(y)(env.SIG_2f1(f)).toTASK))
  def foldR: ENV =
    env.mod3((x, y, z) =>
      z.vec1(f => x.foldRight(y)(env.SIG_2y1f1(f), env.SIG_2f1(f)))
    )
  def rfold: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.rfoldLeft(y)(env.SIG_2f1(f))))
  def rfoldR: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.rfoldRight(y)(env.SIG_2f1(f))))
  def reduce: ENV =
    env.mod2((x, y) =>
      y.vec1(f => x.reduceLeft(env.SIG_1y2f1(f), env.SIG_2f1(f)))
    )
  def reduceR: ENV =
    env.mod2((x, y) =>
      y.vec1(f => x.reduceRight(env.SIG_1y2f1(f), env.SIG_2f1(f)))
    )
  def scan: ENV =
    env.mod3((x, y, z) =>
      z.vec1(f => x.scanLeft(y)(env.SIG_1y2f1(f), env.SIG_2f1(f)))
    )
  def scanEval: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.scanEval(y)(env.SIG_2f1(f)).toOBS))
  def scanR: ENV =
    env.mod3((x, y, z) =>
      z.vec1(f => x.scanRight(y)(env.SIG_2y1f1(f), env.SIG_2f1(f)))
    )

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
    env.mod2((x, y) => y.vec1(f => x.filter(env.SIG_2fb(f), env.SIG_1fb(f))))
  def ofltr: ENV =
    env.mod2((x, y) => y.vec1(f => x.filterEval(env.SIG_1f1(f)).toOBS))

  def any: ENV =
    env.mod2((x, y) =>
      y.vec1(f => x.any(env.SIG_2fb(f), env.SIG_1fb(f)).boolTF)
    )
  def oany: ENV =
    env.mod2((x, y) => y.vec1(f => x.oany(env.SIG_1fb(f)).map(_.boolTF).toTASK))
  def all: ENV =
    env.mod2((x, y) =>
      y.vec1(f => x.all(env.SIG_2fb(f), env.SIG_1fb(f)).boolTF)
    )
  def oall: ENV =
    env.mod2((x, y) => y.vec1(f => x.oall(env.SIG_1fb(f)).map(_.boolTF).toTASK))

  def tkwl: ENV =
    env.mod2((x, y) => y.vec1(f => x.takeWhile(env.SIG_2fb(f), env.SIG_1fb(f))))
  def dpwl: ENV =
    env.mod2((x, y) => y.vec1(f => x.dropWhile(env.SIG_2fb(f), env.SIG_1fb(f))))

  def find: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      x.find(env.SIG_2fb(f), env.SIG_1fb(f)) match
        case Some((a, b)) => Vector(a, b).toARR
        case Some(a: ANY) => a
        case None         => UN
  def ofind: ENV = env.mod2: (x, y) =>
    y.vec1(f => x.ofind(env.SIG_1fb(f)).map(_.getOrElse(UN)).toTASK)

  def findi: ENV =
    env.mod2((x, y) => y.vec1(f => x.findIndex(env.SIG_1fb(f)).pipe(NUM(_))))

  def del: ENV =
    env.mod2((x, y) => y.vec1(f => x.delBy(env.SIG_2fb(f), env.SIG_1fb(f))))

  def uniq: ENV =
    env.mod2((x, y) => y.vec1(f => x.uniqBy(env.SIG_2f1(f), env.SIG_1f1(f))))
  def uniq$ : ENV =
    env.mod2((x, y) =>
      y.vec1(f => x.uniqWith(env.SIG_2x2fb(f), env.SIG_2fb(f)))
    )

  def sort: ENV =
    env.mod2((x, y) => y.vec1(f => x.sortBy(env.SIG_2f1(f), env.SIG_1f1(f))))
  def sort$ : ENV =
    env.mod2((x, y) =>
      y.vec1(f => x.sortWith(env.SIG_2x2fb(f), env.SIG_2fb(f)))
    )

  def part: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      val (a, b) = x.partition(env.SIG_2fb(f), env.SIG_1fb(f))
      Vector(a, b).toARR

  def group: ENV =
    env.mod2((x, y) =>
      y.vec1(f => x.groupBy(env.SIG_2f1(f), env.SIG_1f1(f)).toMAP)
    )
  def ogroup: ENV =
    env.mod2((x, y) => y.vec1(f => x.ogroupBy(env.SIG_1f1(f)).toOBS))

  def span: ENV = env.mod2: (x, y) =>
    y.vec1: f =>
      val (a, b) = x.span(env.SIG_2fb(f), env.SIG_1fb(f))
      Vector(a, b).toARR

  def pack: ENV =
    env.mod2((x, y) =>
      y.vec1(f => x.packWith(env.SIG_2x2fb(f), env.SIG_2fb(f)))
    )

  def union: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.unionWith(y, env.SIG_2fb(f))))
  def intersect: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.intersectWith(y, env.SIG_2fb(f))))
  def diff: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.differWith(y, env.SIG_2fb(f))))
