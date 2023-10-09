package sclin

import monix.eval.Task
import monix.execution.CancelableFuture
import monix.execution.Scheduler.Implicits.global
import scala.collection.immutable.HashMap
import scala.collection.immutable.VectorMap
import scala.concurrent._
import scala.concurrent.duration._
import scala.quoted._
import scala.util.matching.Regex.Match
import scala.util.Failure
import scala.util.Random
import scala.util.Success
import scala.util.Try
import spire.math._
import upickle.default._
import util.chaining._
import ANY._

/** ADT for lin types. */
enum ANY:

  case SEQ(x: SEQW[ANY])
  case ARR(x: ARRW[ANY])
  case MAP(x: MAPW[ANY, ANY])
  case STR(x: String)
  case NUM(x: NUMF)
  case DBL(x: Double)
  case TF(x: Boolean)
  case CMD(x: String)
  case FN(p: PATH, x: SEQW[ANY])
  case ERR(x: Throwable)
  case TASK(x: Task[ANY])
  case FUT(x: FUTW[ANY])
  case TRY(b: Boolean, x: ANY, e: Throwable)
  case UN

  def getType: String = getClass.getSimpleName

  override def toString: String = this match
    case MAP(x) =>
      x.toSeq.map { case (i, a) => i.toString + " " + a.toString }
        .mkString("\n")
    case Sty(x)   => x
    case NUM(x)   => x.toString
    case DBL(x)   => x.toString
    case FN(_, x) => x.mkString(" ")
    case ERR(x)   => x.toString
    case _: TF    => toNUM.toString
    case _: TASK  => "(…)~"
    case FUT(x) =>
      s"(${x.value match
          case Some(t) => t.toTRY.toForm
          case _       => "…"
        })~>"
    case TRY(b, x, e) => (if b then x else e).toString
    case UN           => ""
    case _            => join("")

  def toForm: String = this match
    case _: SEQ => s"[…]"
    case ARR(x) => s"[${x.map(_.toForm).mkString(" ")}]"
    case MAP(x) =>
      s"{${x.toSeq.map { case (i, a) => i.toForm + "=>" + a.toForm }
          .mkString(" ")}}"
    case STR(x) =>
      s"\"${x.map {
          case '\n' => "\\n"
          case '\r' => "\\r"
          case '"'  => "\\\""
          case c    => c
        }.mkString}\""
    case FN(PATH(_, l), x) =>
      val n = l.toString.map(c => "⁰¹²³⁴⁵⁶⁷⁸⁹" (c - '0'))
      s"(…)$n"
    case ERR(x) => s"ERR(${x.getMessage})"
    case TRY(b, x, e) =>
      if b then s"YES(${x.toForm})"
      else s"NO(${ERR(e).toForm})"
    case TF(x) => if x then "$T" else "$F"
    case UN    => "UN"
    case _     => toString

  def cmp(t: ANY): Int = (this, t) match
    case (UN, UN)                 => 0
    case (UN, _)                  => -1
    case (_, UN)                  => 1
    case (TF(x), TF(y)) if x == y => 0
    case (TF(x), _)               => if x then 1 else -1
    case (_, TF(x))               => -t.cmp(this)
    case (Itr(x), _) if !x.toBool => NUM(0).cmp(t)
    case (_, Itr(x)) if !x.toBool => cmp(NUM(0))
    case (Itr(x), _) =>
      val x1 = x.toSEQ.x
      val t1 = t.toSEQ.x
      x1.zip(t1)
        .map { case (a, b) => a.cmp(b) }
        .find(_ != 0)
        .getOrElse(x1.sizeCompare(t1))
    case (_, Itr(_))      => -t.cmp(this)
    case (DBL(x), Nmy(y)) => x.compare(y.toDouble)
    case (Nmy(x), DBL(y)) => -t.cmp(this)
    case (NUM(x), NUM(y)) => x.compare(y)
    case (NUM(x), _) =>
      x.compare(t.toSTR.x.map(_.toInt).applyOrElse(0, _ => 0))
    case (_, _: NUM)      => -t.cmp(this)
    case (Sty(x), Sty(y)) => x.compare(y).sign
    case _                => toSTR.cmp(t.toSTR)

  def eql(t: ANY): Boolean  = cmp(t) == 0
  def eqls(t: ANY): Boolean = cmp(t) == 0 && getType == t.getType

  def toBool: Boolean = this match
    case TF(x)        => x
    case SEQ(x)       => !x.isEmpty
    case ARR(x)       => !x.isEmpty
    case MAP(x)       => !x.isEmpty
    case STR(x)       => !x.isEmpty
    case NUM(x)       => !eql(NUM(0))
    case DBL(x)       => !eql(DBL(0))
    case CMD(x)       => !x.isEmpty
    case FN(_, x)     => !x.isEmpty
    case _: ERR       => false
    case TRY(b, _, _) => b
    case FUT(x)       => x.isCompleted && x.value.get.isSuccess
    case _: TASK      => true
    case UN           => false

  def toTF: TF = this match
    case x: TF => x
    case _     => toBool.boolTF

  def toTry: Try[ANY] = this match
    case TRY(b, x, e) => if b then Success(x) else Failure(e)
    case ERR(x)       => Failure(x)
    case x            => Success(x)

  def toJSON: ujson.Value = this match
    case MAP(x) => x.map { case (k, v) => (k.toString, v.toJSON) }
    case ARR(x) => x.map(_.toJSON)
    case Lsy(_) => toARR.toJSON
    case NUM(x) => x.toDouble
    case TF(x)  => x
    case UN     => null
    case _      => toString

  def length: Int = this match
    case UN     => 0
    case Lsy(x) => x.length
    case ARR(x) => x.length
    case MAP(x) => x.size
    case Sty(x) => x.length
    case _      => -1

  def shape: Vector[Int] = this match
    case Itr(x) => x.length +: x.get(NUM(0)).shape
    case _      => Vector()

  def dshape: Vector[Int] = this match
    case Itr(x) =>
      x.length +: x.foldLeft(Vector[Int]())((a, b) =>
        val b1 = b.dshape
        if a.length < b1.length then b1 else a
      )
    case _ => Vector()

  def rank: Int  = shape.length
  def depth: Int = dshape.length

  def reshape(t: Vector[Int]): ANY =
    def loop(a: Vector[Int])(b: ANY): ANY = a match
      case x +: xs => b.div$$(b.length / x).map(loop(xs)).mItr(this)
      case _       => b
    t match
      case xs :+ _ =>
        val a = flat.rflat
        val a1 =
          if a.toBool then LazyList.continually(a).toSEQ.flat
          else LazyList.continually(UN).toSEQ
        loop(xs)(a1.take(t.product).mItr(this))
      case _ => this

  def paxes: ANY = paxes$(shape.indices.reverse.toVector)

  def paxes$(p: Vector[Int]): ANY =
    val s  = shape
    val pg = p.indices.sortBy(p).to(LazyList) #::: LazyList.continually(-1)
    val p1 = s.indices
      .lazyZip(pg)
      .map { case (a, b) => if b < 0 then a else b }
      .to(LazyList)
    val s1 = p1.map(s)
    cProd(s1.map(0.until(_).to(LazyList)))
      .map(a => getn(p1.map(i => NUM(a(i))).toSEQ))
      .toSEQ
      .mItr(this)
      .reshape(s1.toVector)

  def get(i: ANY): ANY =
    val oi = i.optI
    this match
      case Its(x) if oi != None =>
        val i1 = oi.get
        val i2 = if i1 < 0 then i1 + length else i1
        this match
          case Lsy(x) => x.applyOrElse(i2, _ => UN)
          case ARR(x) => x.applyOrElse(i2, _ => UN)
          case Sty(x) =>
            if i2 < x.length then x(i2).toString.mSTR(this) else UN
          case _ => toARR.get(NUM(i2))
      case MAP(x)       => x.applyOrElse(i, _ => UN)
      case TRY(b, x, e) => if b then x else throw e
      case _            => UN

  def gets(is: ANY): ANY = is.map(get)

  def getn(is: ANY): ANY = is.foldLeft(this)(_.get(_))

  def set(i: ANY, t: ANY): ANY =
    val oi = i.optI
    this match
      case Its(x) if oi != None =>
        val i1 = oi.get
        val i2 = if i1 < 0 then i1 + length else i1
        try
          this match
            case Lsy(x) => x.updated(i2, t).mSEQ(this)
            case ARR(x) => x.updated(i2, t).toARR
            case _      => toARR.set(NUM(i2), t).matchType(this)
        catch
          case _: java.lang.IndexOutOfBoundsException => this
          case e                                      => throw e
      case MAP(x) => x.+(i -> t).toMAP
      case _      => this

  def sets(is: Map[ANY, ANY]): ANY = is.foldLeft(this) { case (a, (i, t)) =>
    a.set(i, t)
  }

  def setn(is: SEQW[ANY], t: ANY): ANY = is match
    case LazyList()  => this
    case LazyList(i) => set(i, t)
    case i #:: is    => setmod(i, _.setn(is, t))

  def setmod(i: ANY, f: ANY => ANY): ANY = set(i, f(get(i)))

  def setmods(is: Map[ANY, ANY => ANY]) = is.foldLeft(this) {
    case (a, (i, f)) => a.setmod(i, f)
  }

  def setmodn(is: SEQW[ANY], f: ANY => ANY): ANY = is match
    case LazyList()  => f(this)
    case LazyList(i) => setmod(i, f)
    case i #:: is    => setmod(i, _.setmodn(is, f))

  def remove(i: ANY): ANY =
    val oi = i.optI
    this match
      case Its(x) if oi != None =>
        val i1 = oi.get
        val i2 = if i1 < 0 then i1 + length else i1
        if i2 >= 0 then
          this match
            case Lsy(x) => x.patch(i2, Nil, 1).mSEQ(this)
            case ARR(x) => x.patch(i2, Nil, 1).toARR
            case _      => toARR.remove(NUM(i2)).matchType(this)
        else this
      case MAP(x) => (x - i).toMAP
      case _      => this

  def add$$(t: ANY): ANY = (this, t) match
    case (UN, y)          => y
    case (x, UN)          => x
    case (ARR(x), ARR(y)) => ARR(x ++ y)
    case (MAP(x), MAP(y)) => MAP(x ++ y)
    case (Lsy(x), Itr(y)) => SEQ(x #::: y.toSEQ.x).matchType(this)
    case (Itr(x), Lsy(y)) => SEQ(x.toSEQ.x #::: y).matchType(t)
    case (Its(_), _)      => snoc(t)
    case (_, Its(_))      => cons(t)
    case (x, y)           => Vector(x).toARR.add$$(y)

  def cons(t: ANY): ANY = t match
    case Lsy(x) => (this #:: x).mSEQ(t)
    case ARR(x) => (this +: x).toARR
    case MAP(x) => (VectorMap(get(NUM(0)) -> get(NUM(1))) ++ x).toMAP
    case Sty(x) => (toString + x).mSTR(t)
    case _      => cons(t.toARR)

  def snoc(t: ANY): ANY = this match
    case Lsy(x) => (x :+ t).mSEQ(this)
    case ARR(x) => (x :+ t).toARR
    case MAP(x) => (x + (t.get(NUM(0)) -> t.get(NUM(1)))).toMAP
    case Sty(x) => (x + t.toString).mSTR(this)
    case _      => toARR.snoc(t)

  def sub$$(t: ANY): ANY = (this, t) match
    case (UN, y)          => y
    case (x, UN)          => x
    case (Lsy(x), Itr(y)) => x.filterNot(y.has).mSEQ(this)
    case (ARR(x), Itr(y)) => x.filterNot(y.has).toARR
    case (MAP(x), MAP(y)) => y.foldLeft(x)(_ - _._1).toMAP
    case (MAP(x), It(y))  => y.foldLeft(x)(_ - _).toMAP
    case (Itr(x), y)      => x.sub$$(Vector(y).toARR)
    case (x, y)           => Vector(x).toARR.sub$$(y)

  def mul$$(t: ANY): ANY = (this, t) match
    case (Itr(x), Itr(y)) => x.zip(y, _ mul$$ _).flat
    case (Lsy(_), y)      => LazyList.fill(y.toInt)(this).toSEQ.flat.matchType(this)
    case (x: ARR, y)      => Vector.fill(y.toInt)(x).toARR.flat
    case (Sty(_), y)      => toARR.mul$$(y).toString.mSTR(this)
    case (x, y)           => Vector(x).toARR.mul$$(y)

  def div$$(t: Int): ANY = this match
    case Lsy(x) => x.grouped(t).map(_.toSEQ).mSEQ(this)
    case ARR(x) => x.grouped(t).map(_.toARR).toSEQ
    case MAP(x) => x.grouped(t).map(_.toMAP).toSEQ
    case Sty(_) => toARR.div$$(t).map(_.toString.mSTR(this)).toSEQ
    case x      => Vector(x).toARR.div$$(t)

  def mod$$(t: ANY): ANY = this match
    case Lsy(x) => x.sliding(t.toInt).map(_.toSEQ).mSEQ(this)
    case ARR(x) => x.sliding(t.toInt).map(_.toARR).toSEQ
    case MAP(x) => x.sliding(t.toInt).map(_.toMAP).toSEQ
    case Sty(_) => toARR.mod$$(t).map(_.toString.mSTR(this)).toSEQ
    case x      => Vector(x).toARR.mod$$(t)

  def take(n: Int): ANY =
    if n < 0 && length != 0 then drop(length + n)
    else
      this match
        case Lsy(x) => x.take(n).mSEQ(this)
        case ARR(x) => ARR(x.take(n))
        case MAP(x) => MAP(x.take(n))
        case Sty(x) => x.take(n).mSTR(this)
        case _      => toARR.take(n)

  def drop(n: Int): ANY =
    if n < 0 && length != 0 then take(length + n)
    else
      this match
        case Lsy(x) => x.drop(n).mSEQ(this)
        case ARR(x) => ARR(x.drop(n))
        case MAP(x) => MAP(x.drop(n))
        case Sty(x) => x.drop(n).mSTR(this)
        case _      => toARR.drop(n)

  def has(t: ANY): Boolean = this match
    case Lsy(x) => x.contains(t)
    case ARR(x) => x.contains(t)
    case MAP(x) => x.contains(t)
    case Sty(x) => x.contains(t.toString)
    case _      => toARR.has(t)

  def shuffle: ANY = this match
    case Lsy(x) => Random.shuffle(x).mSEQ(this)
    case ARR(x) => Random.shuffle(x).toARR
    case MAP(x) => Random.shuffle(x).toMAP
    case Sty(x) => Random.shuffle(x).toString.mSTR(this)
    case _      => toARR.shuffle

  def join(s: String): String = this match
    case Lsy(x) => x.mkString(s)
    case ARR(x) => x.mkString(s)
    case Sty(_) => toARR.join(s)
    case _      => toString

  def combHelper(
      x: Seq[ANY],
      f: Seq[Int] => Iterator[Seq[Int]]
  ): Iterator[Seq[ANY]] = x.zipWithIndex.map(_._2).pipe(f).map(_.map(x))

  def permutations: ANY = this match
    case SEQ(x) => combHelper(x, _.permutations).map(_.toSEQ).toSEQ
    case _      => toSEQ.permutations.map(_.matchType(this))

  def combinations(n: Int): ANY = this match
    case Lsy(x) => x.comb(n).map(_.mSEQ(this)).toSEQ
    case ARR(x) => combHelper(x, _.combinations(n)).map(_.toARR).toSEQ
    case _      => toARR.combinations(n).map(_.matchType(this))

  def powset: ANY = this match
    case SEQ(x) =>
      val x1 = x.zipWithIndex.map(_._2)
      x1.flatMap(a => x1.comb(a + 1))
        .pipe(LazyList() #:: _)
        .map(_.map(x).toSEQ)
        .toSEQ
    case _ => toSEQ.powset.map(_.matchType(this))

  def toSEQ: SEQ = this match
    case Lsy(x) => x.toSEQ
    case ARR(x) => x.toSEQ
    case _      => toARR.toSEQ

  def mSEQ(t: ANY): ANY = t match
    case Lsy(_) => matchType(t)
    case _      => toSEQ

  def mSTR(t: ANY): ANY = t match
    case Sty(_) => matchType(t)
    case _      => toSTR

  def toARR: ARR = this match
    case UN     => Vector().toARR
    case x: ARR => x
    case Lsy(x) => x.toARR
    case MAP(x) => x.toVector.map { case (i, a) => Vector(i, a).toARR }.toARR
    case Sty(x) => x.toVector.map(c => c.toString.mSTR(this)).toARR
    case _      => Vector(this).toARR

  def toMAP: MAP = this match
    case x: MAP => x
    case ARR(x) =>
      x.flatMap {
        case Itr(a) if a.length > 0 => Some((a.get(NUM(0)), a.get(NUM(1))))
        case _                      => None
      }.to(VectorMap).toMAP
    case _ => toARR.toMAP

  def toSTR: STR = STR(toString)

  def toNUM: NUM =
    try
      this match
        case UN     => NUM(0)
        case x: NUM => x
        case TF(x)  => x.boolNUM
        case Sty(x) => x.toNUM
        case _      => toSTR.x.toNUM
    catch
      case e: java.lang.NumberFormatException =>
        throw LinEx("CAST", "bad NUM cast " + toForm)
      case e => throw e

  def toDBL: DBL = DBL(toDouble)

  def toDouble: Double = toNUM.x.toDouble

  def toThrow: Throwable = this match
    case ERR(x) => x
    case x      => LinEx("_", x.toString)

  def toERR: ERR = this match
    case x: ERR => x
    case _      => toThrow.pipe(ERR(_))

  def toTRY: TRY = this match
    case x: TRY => x
    case _      => toTry.toTRY

  def optNUM: Option[NUM] =
    try Some(toNUM)
    catch _ => None

  def toInt: Int = toNUM.x.intValue

  def optI: Option[Int] = optNUM.map(_.x.intValue)

  def xFN: LazyList[ANY] = this match
    case Lsy(x) => x
    case Sty(x) => Parser.parse(x)
    case _      => toSEQ.xFN

  def toFN(env: ENV): FN = FN(env.code.p, xFN)

  def lFN(l: Int, env: ENV): FN = FN(PATH(env.code.p.f, l), xFN)

  def pFN(p: PATH): FN = FN(p, xFN)

  def toTASK: TASK = this match
    case x: TASK => x
    case FUT(x)  => Task.fromFuture(x).pipe(TASK(_))
    case x       => Task.pure(x).pipe(TASK(_))

  def modTASK(f: Task[ANY] => Task[ANY]): TASK =
    toTASK.x.pipe(f).pipe(TASK(_))

  def toFUT: FUT = this match
    case x: FUT  => x
    case TASK(x) => x.runToFuture.pipe(FUT(_))
    case x       => CancelableFuture.pure(x).pipe(FUT(_))

  def matchType(a: ANY): ANY = a match
    case FN(p, _) => pFN(p)
    case _: SEQ   => toSEQ
    case _: ARR   => toARR
    case _: MAP   => toMAP
    case _: STR   => toSTR
    case _: NUM   => toNUM
    case _: DBL   => toDBL
    case _: TF    => toTF
    case _: CMD   => toString.pipe(CMD(_))
    case _: TASK  => toTASK
    case _: FUT   => toFUT
    case _: TRY   => toTRY
    case _: ERR   => toERR
    case UN       => UN

  def toItr: ANY = this match
    case Lsy(_) | Itr(_) => this
    case _               => toARR

  def mItr(a: ANY): ANY = a match
    case Lsy(_) | Itr(_) => matchType(a)
    case _               => toARR

  def mIts(a: ANY): ANY = a match
    case Its(_) => matchType(a)
    case _      => toARR

  def map(f: ANY => ANY): ANY = this match
    case Lsy(x)  => x.map(f).mSEQ(this)
    case ARR(x)  => x.map(f).toARR
    case TASK(x) => x.map(f).pipe(TASK(_))
    case FUT(x)  => x.map(f).pipe(FUT(_))
    case x: TRY  => x.toTry.map(f).toTRY
    case _       => toARR.map(f)
  def mapM(f: (ANY, ANY) => (ANY, ANY), g: ANY => ANY): ANY = this match
    case MAP(x) => x.map(f.tupled).toMAP
    case _      => map(g)

  def flatMap(f: ANY => ANY): ANY = this match
    case Lsy(x)  => x.flatMap(f(_).toSEQ.x).mSEQ(this)
    case ARR(x)  => x.flatMap(f(_).toARR.x).toARR
    case TASK(x) => x.flatMap(f(_).toTASK.x).pipe(TASK(_))
    case FUT(x)  => x.map(f).pipe(FUT(_))
    case x: TRY  => x.toTry.flatMap(f(_).toTry).toTRY
    case _       => toARR.flatMap(f)
  def flatMapM(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) => x.flatMap { case (a, b) => f(a, b).toARR.x }.toARR
    case _      => flatMap(g)
  def flatMap$(f: ANY => ARRW[ANY]): ANY = this match
    case Lsy(x) => x.flatMap(f).mSEQ(this)
    case ARR(x) => x.flatMap(f).toARR
    case _      => toARR.flatMap$(f)
  def flatMap$M(f: (ANY, ANY) => ARRW[ANY], g: ANY => ARRW[ANY]): ANY =
    this match
      case MAP(x) =>
        x.flatMap { case (a, b) =>
          f(a, b) match
            case _ :+ k :+ v => Some(k, v)
            case Vector(v)   => Some(a, v)
            case _           => None
        }.toMAP
      case _ => flatMap$(g)
  def flat: ANY = flatMap(x => x)
  def rflat: ANY = this match
    case Itr(_) => flatMap(_.rflat)
    case x      => x

  def zip(t: ANY, f: (ANY, ANY) => ANY): ANY = (this, t) match
    case (Lsy(x), _)   => x.zip(t.toSEQ.x).map(f.tupled).mSEQ(this)
    case (_, Lsy(_))   => toSEQ.zip(t, f).mSEQ(t)
    case (FN(p, x), _) => x.lazyZip(t.toARR.x).map(f).pFN(p)
    case _             => toARR.x.lazyZip(t.toARR.x).map(f).toARR

  def zipAll(t: ANY, d1: ANY, d2: ANY, f: (ANY, ANY) => ANY): ANY =
    (this, t) match
      case (Lsy(x), _) =>
        x.zipAll(t.toSEQ.x, d1, d2).map(f.tupled).mSEQ(this)
      case (_, Lsy(_)) => toSEQ.zipAll(t, d1, d2, f).mSEQ(t)
      case _           => toARR.x.zipAll(t.toARR.x, d1, d2).map(f.tupled).toARR

  def table(t: ANY, f: (ANY, ANY) => ANY): ANY = map(x => t.map(y => f(x, y)))
  def flatTable(t: ANY, f: (ANY, ANY) => ANY): ANY =
    flatMap(x => t.map(y => f(x, y)))

  def rmap(f: ANY => ANY): ANY = this match
    case Itr(_) => mapM((k, v) => (k, v.rmap(f)), _.rmap(f))
    case x      => f(x)

  def toShape(t: ANY): ANY =
    val q = toSEQ.rflat.toSEQ.x
    var f = LazyList.continually(q).flatten
    t.rmap(_ =>
      f match
        case x #:: xs =>
          f = xs
          x
        case _ => UN
    )

  def foldLeft[T](a: T)(f: (T, ANY) => T): T = this match
    case Lsy(x) => x.foldLeft(a)(f)
    case ARR(x) => x.foldLeft(a)(f)
    case _      => toARR.foldLeft(a)(f)
  def foldLeftM[T](a: T)(f: (T, (ANY, ANY)) => T, g: (T, ANY) => T): T =
    this match
      case MAP(x) => x.foldLeft(a)((b, c) => f(b, c))
      case _      => foldLeft(a)(g)

  def foldRight[T](a: T)(f: (ANY, T) => T): T = this match
    case Lsy(x) => x.foldRight(a)(f)
    case ARR(x) => x.foldRight(a)(f)
    case _      => toARR.foldRight(a)(f)
  def foldRightM[T](a: T)(f: ((ANY, ANY), T) => T, g: (ANY, T) => T): T =
    this match
      case MAP(x) => x.foldRight(a)(f)
      case _      => foldRight(a)(g)

  def rfoldLeft[T](a: T)(f: (T, ANY) => T): T =
    this match
      case Itr(_) =>
        foldLeftM(a)(
          { case (s, (_, v)) => v.rfoldLeft(s)(f) },
          (s, v) => v.rfoldLeft(s)(f)
        )
      case x => f(a, x)

  def rfoldRight[T](a: T)(f: (T, ANY) => T): T =
    this match
      case Itr(_) =>
        foldRightM(a)(
          { case ((_, v), s) => v.rfoldRight(s)(f) },
          (v, s) => v.rfoldRight(s)(f)
        )
      case x => f(a, x)

  def reduceLeft(f: (ANY, ANY) => ANY): ANY =
    try
      this match
        case Lsy(x) => x.reduceLeft(f).matchType(this)
        case ARR(x) => x.reduceLeft(f)
        case _      => toARR.reduceLeft(f)
    catch
      case e: java.lang.UnsupportedOperationException =>
        throw LinEx("ITR", s"unable to reduce empty $getType")
      case e => throw e
  def reduceLeftM(f: (ANY, (ANY, ANY)) => ANY, g: (ANY, ANY) => ANY): ANY =
    this match
      case _: MAP =>
        toARR.reduceLeft {
          case (a, ARR(k +: v +: _)) => f(a, (k, v))
          case _                     => ???
        }
      case _ => reduceLeft(g)

  def reduceRight(f: (ANY, ANY) => ANY): ANY =
    try
      this match
        case Lsy(x) => x.reduceRight(f).matchType(this)
        case ARR(x) => x.reduceRight(f)
        case _      => toARR.reduceRight(f)
    catch
      case e: java.lang.UnsupportedOperationException =>
        throw LinEx("ITR", s"unable to reduce empty $getType")
      case e => throw e
  def reduceRightM(f: (ANY, (ANY, ANY)) => ANY, g: (ANY, ANY) => ANY): ANY =
    this match
      case _: MAP =>
        toARR.reduceRight {
          case (a, ARR(k +: v +: _)) => f(a, (k, v))
          case _                     => ???
        }
      case _ => reduceRight(g)

  def scanLeft(a: ANY)(f: (ANY, ANY) => ANY): ANY = this match
    case Lsy(x) => x.scanLeft(a)(f).mSEQ(this)
    case ARR(x) => x.scanLeft(a)(f).toARR
    case _      => toARR.scanLeft(a)(f)
  def scanLeftM(
      a: ANY
  )(f: (ANY, (ANY, ANY)) => ANY, g: (ANY, ANY) => ANY): ANY = this match
    case MAP(x) => x.scanLeft(a)(f).toARR
    case _      => scanLeft(a)(g)

  def scanRight(a: ANY)(f: (ANY, ANY) => ANY): ANY = this match
    case Lsy(x) => x.scanRight(a)(f).mSEQ(this)
    case ARR(x) => x.scanRight(a)(f).toARR
    case _      => toARR.scanRight(a)(f)
  def scanRightM(
      a: ANY
  )(f: ((ANY, ANY), ANY) => ANY, g: (ANY, ANY) => ANY): ANY = this match
    case MAP(x) => x.scanRight(a)(f).toARR
    case _      => scanRight(a)(g)

  def filter(f: ANY => Boolean): ANY = this match
    case Lsy(x) => x.filter(f).mSEQ(this)
    case ARR(x) => x.filter(f).toARR
    case x: TRY => x.toTry.filter(f).toTRY
    case _      => toARR.filter(f)
  def filterM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.filter(f.tupled).toMAP
    case _      => filter(g)

  def any(f: ANY => Boolean): Boolean = this match
    case Lsy(x) => x.exists(f)
    case ARR(x) => x.exists(f)
    case _      => toARR.any(f)
  def anyM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): Boolean = this match
    case MAP(x) => x.exists(f.tupled)
    case _      => any(g)

  def all(f: ANY => Boolean): Boolean = this match
    case Lsy(x) => x.forall(f)
    case ARR(x) => x.forall(f)
    case _      => toARR.all(f)
  def allM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): Boolean = this match
    case MAP(x) => x.forall(f.tupled)
    case _      => all(g)

  def takeWhile(f: ANY => Boolean): ANY = this match
    case Lsy(x) => x.takeWhile(f).mSEQ(this)
    case ARR(x) => x.takeWhile(f).toARR
    case _      => toARR.takeWhile(f)
  def takeWhileM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.takeWhile(f.tupled).toMAP
    case _      => takeWhile(g)

  def dropWhile(f: ANY => Boolean): ANY = this match
    case Lsy(x) => x.dropWhile(f).mSEQ(this)
    case ARR(x) => x.dropWhile(f).toARR
    case _      => toARR.dropWhile(f)
  def dropWhileM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.dropWhile(f.tupled).toMAP
    case _      => dropWhile(g)

  def find(f: ANY => Boolean): Option[ANY] = this match
    case Lsy(x) => x.find(f)
    case ARR(x) => x.find(f)
    case _      => toARR.find(f)
  def findM(
      f: (ANY, ANY) => Boolean,
      g: ANY => Boolean
  ): Option[ANY | (ANY, ANY)] = this match
    case MAP(x) => x.find(f.tupled)
    case _      => find(g)

  def findIndex(f: ANY => Boolean): Int = this match
    case Lsy(x) => x.indexWhere(f)
    case ARR(x) => x.indexWhere(f)
    case _      => toARR.findIndex(f)

  def delBy(f: ANY => Boolean): ANY = this match
    case Lsy(x) => x.delBy(f).mSEQ(this)
    case ARR(x) => x.delBy(f).toARR
    case _      => toARR.delBy(f)
  def delByM(
      f: (ANY, ANY) => Boolean,
      g: ANY => Boolean
  ): ANY = this match
    case MAP(x) => x.delBy(f.tupled).to(VectorMap).toMAP
    case _      => delBy(g)

  def uniqBy(f: ANY => ANY): ANY = this match
    case Lsy(x) => x.distinctBy(f).mSEQ(this)
    case ARR(x) => x.distinctBy(f).toARR
    case _      => toARR.uniqBy(f)
  def uniqByM(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) =>
      x.toSeq.distinctBy(f.tupled).to(VectorMap).toMAP
    case _ => uniqBy(g)

  def uniqWith(f: (ANY, ANY) => Boolean): ANY = this match
    case Lsy(x) => x.uniqWith(f).mSEQ(this)
    case ARR(x) => x.uniqWith(f).toARR
    case _      => toARR.uniqWith(f)
  def uniqWithM(
      f: ((ANY, ANY), (ANY, ANY)) => Boolean,
      g: (ANY, ANY) => Boolean
  ): ANY = this match
    case MAP(x) => x.toSeq.uniqWith(f).to(VectorMap).toMAP
    case _      => uniqWith(g)

  def sortBy(f: ANY => ANY): ANY = this match
    case Lsy(x) => x.sortBy(f)(OrdANY).mSEQ(this)
    case ARR(x) => x.sortBy(f)(OrdANY).toARR
    case _      => toARR.sortBy(f)
  def sortByM(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) =>
      x.toSeq.sortBy(f.tupled)(OrdANY).to(VectorMap).toMAP
    case _ => sortBy(g)

  def sortWith(f: (ANY, ANY) => Boolean): ANY = this match
    case Lsy(x) => x.sortWith(f).mSEQ(this)
    case ARR(x) => x.sortWith(f).toARR
    case _      => toARR.sortWith(f)
  def sortWithM(
      f: ((ANY, ANY), (ANY, ANY)) => Boolean,
      g: (ANY, ANY) => Boolean
  ): ANY = this match
    case MAP(x) => x.toSeq.sortWith(f).to(VectorMap).toMAP
    case _      => sortWith(g)

  def partition(f: ANY => Boolean): (ANY, ANY) = this match
    case Lsy(x) =>
      x.partition(f).pipe { case (a, b) => (a.mSEQ(this), b.mSEQ(this)) }
    case ARR(x) => x.partition(f).pipe { case (a, b) => (a.toARR, b.toARR) }
    case _      => toARR.partition(f)
  def partitionM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): (ANY, ANY) =
    this match
      case MAP(x) =>
        x.partition(f.tupled).pipe { case (a, b) => (a.toMAP, b.toMAP) }
      case _ => partition(g)

  def groupBy(f: ANY => ANY): Map[ANY, ANY] = this match
    case Lsy(x) => x.groupBy(f).view.mapValues(_.mSEQ(this)).toMap
    case ARR(x) => x.groupBy(f).view.mapValues(_.toARR).toMap
    case _      => toARR.groupBy(f)
  def groupByM(f: (ANY, ANY) => ANY, g: ANY => ANY): Map[ANY, ANY] = this match
    case MAP(x) =>
      x.groupBy(f.tupled).view.mapValues(_.toMAP).toMap
    case _ => groupBy(g)

  def span(f: ANY => Boolean): (ANY, ANY) = this match
    case Lsy(x) =>
      x.span(f).pipe { case (a, b) => (a.mSEQ(this), b.mSEQ(this)) }
    case ARR(x) => x.span(f).pipe { case (a, b) => (a.toARR, b.toARR) }
    case _      => toARR.span(f)
  def spanM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): (ANY, ANY) =
    this match
      case MAP(x) => x.span(f.tupled).pipe { case (a, b) => (a.toMAP, b.toMAP) }
      case _      => span(g)

  def packWith(f: (ANY, ANY) => Boolean): ANY = this match
    case Lsy(x) => x.packWith(f).map(_.mSEQ(this)).mSEQ(this)
    case ARR(x) => x.packWith(f).map(_.toARR).toARR
    case _      => toARR.packWith(f)
  def packWithM(
      f: ((ANY, ANY), (ANY, ANY)) => Boolean,
      g: (ANY, ANY) => Boolean
  ): ANY = this match
    case MAP(x) => x.toSeq.packWith(f).map(_.to(VectorMap).toMAP).toSEQ
    case _      => packWith(g)

  def unionWith(t: ANY, f: (ANY, ANY) => Boolean): ANY = (this, t) match
    case (Lsy(x), _) => x.unionWith(t.toSEQ.x, f).mSEQ(this)
    case (_, Lsy(_)) => toSEQ.unionWith(t, f).matchType(t)
    case _           => toARR.x.unionWith(t.toARR.x, f).toARR

  def intersectWith(t: ANY, f: (ANY, ANY) => Boolean): ANY = (this, t) match
    case (Lsy(x), _) => x.intersectWith(t.toSEQ.x, f).mSEQ(this)
    case (_, Lsy(_)) => toSEQ.intersectWith(t, f).matchType(t)
    case _           => toARR.x.intersectWith(t.toARR.x, f).toARR

  def differWith(t: ANY, f: (ANY, ANY) => Boolean): ANY = (this, t) match
    case (Lsy(x), _) => x.differWith(t.toSEQ.x, f).mSEQ(this)
    case (_, Lsy(_)) => toSEQ.differWith(t, f).matchType(t)
    case _           => toARR.x.differWith(t.toARR.x, f).toARR

  def vec1(f: ANY => ANY): ANY = this match
    case Itr(_) => map(_.vec1(f))
    case _      => f(this)

  def vec2(t: ANY, f: (ANY, ANY) => ANY): ANY = (this, t) match
    case (Itr(_), Itr(_)) => zip(t, _.vec2(_, f))
    case (Itr(_), _)      => vec1(f(_, t))
    case (_, Itr(_))      => t.vec1(f(this, _))
    case _                => f(this, t)

  def vec3(t: ANY, s: ANY, f: (ANY, ANY, ANY) => ANY): ANY = (this, t, s) match
    case (Itr(_), Itr(_), Itr(_)) =>
      zip(t, Vector(_, _).toARR).zip(
        s,
        (x, c) =>
          x.toARR.x match
            case Vector(a, b) => a.vec3(b, c, f)
            case _            => ???
      )
    case (Itr(_), Itr(_), _) => vec2(t, f(_, _, s))
    case (Itr(_), _, Itr(_)) => vec2(t, f(_, t, _))
    case (_, Itr(_), Itr(_)) => t.vec2(s, f(this, _, _))
    case (Itr(_), _, _)      => vec1(f(_, t, s))
    case (_, Itr(_), _)      => t.vec1(f(this, _, s))
    case (_, _, Itr(_))      => s.vec1(f(this, t, _))
    case _                   => f(this, t, s)

  def vef1[T](a: T)(f: (T, ANY) => T): T = this match
    case Itr(_) => foldLeft(a)((x, y) => y.vef1(x)(f))
    case x      => f(a, x)

  def num1(f: Double => Double, g: NUMF => NUMF): ANY =
    vec1(_.fNum1(f(_).toDBL, g(_).toNUM))
  def num1(f: Double => Double, g: NUMF => NUMF, e: String): ANY =
    try num1(f, g)
    catch case _: ArithmeticException => throw LinEx("MATH", e)

  def num2(
      t: ANY,
      f: (Double, Double) => Double,
      g: (NUMF, NUMF) => NUMF
  ): ANY =
    vec2(t, _.fNum2(_, f(_, _).toDBL, g(_, _).toNUM))
  def num2(
      t: ANY,
      f: (Double, Double) => Double,
      g: (NUMF, NUMF) => NUMF,
      e: String
  ): ANY =
    try num2(t, f, g)
    catch case _: ArithmeticException => throw LinEx("MATH", e)

  def num2q(t: ANY, f: (NUMF, NUMF) => Iterable[NUMF]): ANY =
    vec2(t, (x, y) => f(x.toNUM.x, y.toNUM.x).map(NUM(_)).toSEQ)

  def num2a(t: ANY, f: (NUMF, NUMF) => Iterable[NUMF]): ANY =
    vec2(t, (x, y) => f(x.toNUM.x, y.toNUM.x).map(NUM(_)).toARR)

  def str1(f: String => String): ANY = vec1(_.toString.pipe(f).sSTR)

  def str1a(f: String => Iterable[String]): ANY = vec1(
    _.toString.pipe(f).map(STR(_)).toARR
  )

  def str2(t: ANY, f: (String, String) => String): ANY =
    vec2(t, (x, y) => f(x.toString, y.toString).sSTR)

  def str2q(t: ANY, f: (String, String) => Iterable[String]): ANY =
    vec2(t, (x, y) => f(x.toString, y.toString).map(_.sSTR).toSEQ)

  def str2a(t: ANY, f: (String, String) => Iterable[String]): ANY =
    vec2(t, (x, y) => f(x.toString, y.toString).map(_.sSTR).toARR)

  def strnum(t: ANY, f: (String, NUMF) => String): ANY =
    vec2(t, (x, y) => STR(f(x.toString, y.toNUM.x)))

  def strnumq(t: ANY, f: (String, NUMF) => Iterable[String]): ANY =
    vec2(t, (x, y) => f(x.toString, y.toNUM.x).map(_.sSTR).toSEQ)

  def strnuma(t: ANY, f: (String, NUMF) => Iterable[String]): ANY =
    vec2(t, (x, y) => f(x.toString, y.toNUM.x).map(_.sSTR).toARR)

  def fNum1(f: Double => ANY, g: NUMF => ANY) = this match
    case DBL(x) => f(x.toDouble)
    case _      => g(toNUM.x)

  def fNum2(t: ANY, f: (Double, Double) => ANY, g: (NUMF, NUMF) => ANY) =
    (this, t) match
      case (DBL(x), Nmy(y)) => f(x, y.toDouble)
      case (Nmy(x), DBL(y)) => f(x.toDouble, y)
      case (x, y)           => g(x.toNUM.x, y.toNUM.x)

given ReadWriter[ANY] = readwriter[ujson.Value].bimap[ANY](_.toJSON, _.toANY)

object OrdANY extends Ordering[ANY]:

  def compare(x: ANY, y: ANY): Int = x.cmp(y)

object ANY:

  extension [T](xs: Iterable[T])

    def packWith(f: (T, T) => Boolean): Iterable[Iterable[T]] =
      def loop(
          xs: Iterable[T],
          res: Iterable[Iterable[T]] = Iterable()
      ): Iterable[Iterable[T]] =
        if xs.isEmpty then res
        else
          val (y, ys) = (xs.head, xs.tail)
          val (z, zs) = ys.span(f(y, _))
          loop(zs, res ++ Iterable(Iterable(y) ++ z))
      loop(xs)

    def uniqWith(f: (T, T) => Boolean): Iterable[T] =
      xs.foldLeft(Iterable[T]())((acc, x) =>
        if acc.exists(f(x, _)) then acc else acc ++ Iterable(x)
      )

    def delBy(f: T => Boolean): Iterable[T] =
      val (x, y) = xs.span(f)
      x ++ y.drop(1)

    def unionWith(ys: Iterable[T], f: (T, T) => Boolean): Iterable[T] =
      xs ++ ys.uniqWith(f).filter(y => !xs.exists(f(_, y)))

    def intersectWith(ys: Iterable[T], f: (T, T) => Boolean): Iterable[T] =
      if xs.isEmpty || ys.isEmpty then Iterable()
      else xs.filter(x => ys.exists(f(x, _)))

    def differWith(ys: Iterable[T], f: (T, T) => Boolean): Iterable[T] =
      if xs.isEmpty then ys
      else if ys.isEmpty then xs
      else xs.filter(x => !ys.exists(f(x, _)))

  extension [T](xs: SEQW[T])

    def packWith(f: (T, T) => Boolean): SEQW[SEQW[T]] = xs match
      case LazyList() => LazyList()
      case x #:: xs =>
        val (ys, zs) = xs.span(f(x, _))
        (x #:: ys) #:: zs.packWith(f)

    def uniqWith(f: (T, T) => Boolean): SEQW[T] = xs match
      case LazyList() => LazyList()
      case x #:: xs   => x #:: xs.filter(!f(x, _)).uniqWith(f)

    def delBy(f: T => Boolean): SEQW[T] = xs match
      case LazyList() => LazyList()
      case x #:: xs   => if f(x) then xs else x #:: xs.delBy(f)

    def unionWith(ys: SEQW[T], f: (T, T) => Boolean): SEQW[T] =
      xs #::: ys.uniqWith(f).filter(y => !xs.exists(f(_, y)))

    def comb(n: Int): SEQW[SEQW[T]] =
      if n < 0 then LazyList()
      else if n == 0 then LazyList(LazyList())
      else
        xs match
          case LazyList() => LazyList()
          case x #:: xs   => xs.comb(n - 1).map(x #:: _) #::: xs.comb(n)

  extension (x: Iterable[ANY])

    def toSEQ: SEQ = SEQ(x.to(LazyList))
    def mSEQ(t: ANY): ANY = t match
      case Lsy(_) => toSEQ.matchType(t)
      case _      => toSEQ
    def toARR: ARR       = ARR(x.toVector)
    def pFN(p: PATH): FN = FN(p, x.to(LazyList))

  extension (x: Iterator[ANY])

    def toSEQ: SEQ = SEQ(x.to(LazyList))
    def mSEQ(t: ANY): ANY = t match
      case Lsy(_) => toSEQ.matchType(t)
      case _      => toSEQ
    def pFN(p: PATH): FN = FN(p, x.to(LazyList))

  extension (x: Map[ANY, ANY]) def toMAP: MAP = MAP(x.to(VectorMap))

  extension (b: Boolean)

    def boolInt: Int = if b then 1 else 0
    def boolNUM: NUM = NUM(b.boolInt)
    def boolTF: TF   = TF(b)

  extension (s: String)

    def toNUM: NUM = NUM(Real(s))
    def sSTR: STR  = STR(s)
    def mSTR(t: ANY): ANY = t match
      case Sty(_) => sSTR.matchType(t)
      case _      => sSTR
    def strun: ANY = s match
      case null => UN
      case _    => STR(s)

  extension (t: Try[ANY])

    def toTRY: TRY = t match
      case Success(s) => TRY(true, s, LinEx("_", ""))
      case Failure(e) => TRY(false, UN, e)

  extension (t: Throwable)

    def toERRW(env: ENV): ERR = t match
      case e: LinERR => ERR(e)
      case e: LinEx  => e.toLinERR(env).toERRW(env)
      case e         => LinEx("_", e.getMessage).toERRW(env)

  extension (v: ujson.Value)

    def toANY: ANY = v match
      case ujson.Obj(x) =>
        x.map { case (k, v) => (STR(k), v.toANY) }.to(VectorMap).toMAP
      case ujson.Arr(x) => x.map(_.toANY).toARR
      case ujson.Str(x) => STR(x)
      case ujson.Num(x) => NUM(x)
      case ujson.True   => TF(true)
      case ujson.False  => TF(false)
      case ujson.Null   => UN

  extension (m: Match)

    def matchMAP: MAP =
      def strun(s: CharSequence): ANY =
        Option(s).map(_.toString) match
          case Some(x) => STR(x)
          case _       => UN

      VectorMap(
        STR("&") -> m.matched.strun,
        STR("`") -> m.before.pipe(strun),
        STR("'") -> m.after.pipe(strun),
        STR("*") ->
          m.subgroups.zipWithIndex.map { case (g, i) =>
            val i1 = i + 1
            VectorMap(
              STR("&") -> g.strun,
              STR("`") -> m.before(i1).pipe(strun),
              STR("'") -> m.after(i1).pipe(strun),
              STR("^") -> NUM(m.start(i1)),
              STR("$") -> NUM(m.end(i1))
            ).toMAP
          }.toARR,
        STR("^") -> NUM(m.start),
        STR("$") -> NUM(m.end)
      ).toMAP

  extension (n: Double) def toDBL: DBL = DBL(n)

  extension (n: Real) def toNUM: NUM = NUM(n)

  /** Pattern for `SEQ`-like. */
  object Itr:

    def unapply(a: ANY): Option[ANY] = a match
      case _: SEQ | _: ARR | _: MAP => Some(a)
      case _                        => None

  /** Pattern for strict `SEQ`-like. */
  object It:

    def unapply(a: ANY): Option[ANY] = a match
      case _: SEQ | _: ARR => Some(a)
      case _               => None

  /** Pattern for loose `SEQ`-like. */
  object Its:

    def unapply(a: ANY): Option[ANY] = a match
      case Lsy(_) | _: ARR | Sty(_) | _: FN => Some(a)
      case _                                => None

  object Lsy:

    def unapply(a: ANY): Option[LazyList[ANY]] = a match
      case SEQ(x)   => Some(x)
      case FN(_, x) => Some(x)
      case _        => None

  object Nmy:

    def unapply(a: ANY): Option[ANY] = a match
      case _: NUM | _: DBL => Some(a)
      case _               => None

  object Sty:

    def unapply(a: ANY): Option[String] = a match
      case STR(x) => Some(x)
      case CMD(x) => Some(x)
      case _      => None

  def fromDec(n: SafeLong, b: Int): ARRW[SafeLong] =
    def loop(
        n: SafeLong,
        b: Int = b,
        res: ARRW[SafeLong] = Vector()
    ): ARRW[SafeLong] =
      if b == 0 then Vector()
      else if b == 1 then Vector.fill(n.toInt)(1)
      else if b < 0 then loop(n, -b).reverse
      else if n == 0 then
        res match
          case Vector() if b > 1 => Vector(0)
          case x                 => x
      else loop(n / b, b, n % b +: res)
    loop(n)

  def toDec(ns: ARRW[SafeLong], b: SafeLong): SafeLong =
    if b == 0 then 0
    else if b < 0 then toDec(ns.reverse, -b)
    else if b == 1 then ns.length
    else
      ns.reverseIterator.zipWithIndex.foldLeft[SafeLong](0) {
        case (a, (n, i)) =>
          b ** i * n + a
      }

  def cProd[A](ls: SEQW[SEQW[A]]): SEQW[SEQW[A]] = ls match
    case LazyList()       => LazyList()
    case x #:: LazyList() => x.map(LazyList(_))
    case x #:: xs =>
      val y = cProd(xs)
      x.flatMap(a => y.map(b => a #:: b))

  def cPow[A](seed: SEQW[A], n: Int): SEQW[SEQW[A]] = cProd(
    LazyList.fill(n)(seed)
  )

  def transpose[A](ls: SEQW[SEQW[A]]): SEQW[SEQW[A]] =
    ls.filter(_.nonEmpty) match
      case LazyList() => LazyList()
      case xs         => xs.map(_.head) #:: transpose(xs.map(_.tail))
