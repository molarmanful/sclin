package sclin

import better.files.*
import cats.effect.ExitCase
import cats.kernel.Eq
import monix.eval.Task
import monix.execution.*
import monix.execution.Scheduler.Implicits.global
import monix.reactive.*
import scala.annotation.tailrec
import scala.collection.immutable.VectorMap
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.matching.Regex.Match
import scala.util.Failure
import scala.util.Random
import scala.util.Success
import scala.util.Try
import spire.math.*
import upickle.default.*
import util.chaining.*
import ANY.*

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
  case FN(p: PATH, s: SCOPE, x: SEQW[ANY])
  case ERR(x: Throwable)
  case TASK(x: Task[ANY])
  case FUT(x: FUTW[ANY])
  case TRY(x: Try[ANY])
  case OBS(x: Observable[ANY])
  case OSTRAT(x: OverflowStrategy[ANY])
  case UN

  def getType: String = getClass.getSimpleName

  override def toString: String = this match
    case MAP(x) =>
      x.toSeq.map { case (i, a) => i.toString + " " + a.toString }
        .mkString("\n")
    case Sty(x)      => x
    case NUM(x)      => x.toString
    case DBL(x)      => x.toString
    case FN(_, _, x) => x.mkString(" ")
    case ERR(x)      => x.toString
    case _: TF       => toNUM.toString
    case FUT(x) =>
      x.value.map(_.toTRY.toString).getOrElse("")
    case _: TASK         => toFUT.toString
    case _: OBS          => toARR.toString
    case TRY(Success(x)) => x.toString
    case TRY(Failure(e)) => e.toString
    case UN              => ""
    case OSTRAT(x)       => x.toString
    case _               => join("")

  def toForm: String = toFormB(_.toForm)

  def toFormB(form: ANY => String): String = this match
    case _: SEQ => s"[…]"
    case ARR(x) => s"[${x.map(form).mkString(" ")}]"
    case MAP(x) =>
      s"[${x.toSeq
          .map:
            case (i, a) => form(i) + "=>" + form(a)
          .mkString(" ")}]:"
    case STR(x) =>
      s"\"${x
          .map:
            case '\t' => "\\t"
            case '\b' => "\\b"
            case '\n' => "\\n"
            case '\r' => "\\r"
            case '\f' => "\\f"
            case '"'  => "\\\""
            case '\\' => "\\\\"
            case c    => c
          .mkString}\""
    case FN(p, _, _)     => s"($p)"
    case ERR(x)          => s"ERR(${x.getMessage})"
    case TRY(Success(x)) => s"YES(${form(x)})"
    case TRY(Failure(e)) => s"NO(${ERR(e).pipe(form)})"
    case TF(x)           => if x then "$T" else "$F"
    case _: TASK         => "(…)~"
    case _: OBS          => "[…]~"
    case FUT(x) =>
      s"(${x.value match
          case Some(t) => t.toTRY.pipe(form)
          case _       => "…"
        })~>"
    case UN => "UN"
    case _  => toString

  def toFormInd(n: Int = 0): String =
    val n1 = n + 1
    this match
      case ARR(x)
          if x
            .exists:
              case _: ARR | _: MAP => true
              case _               => false
          =>
        s"[\n${x.map("\t" * n1 + _.toFormInd(n1)).mkString("\n")}\n${"\t" * n}]"
      case MAP(x) =>
        s"[\n${x.toSeq
            .map:
              case (i, a) =>
                "\t" * n1 + s"${i.toFormInd(n1)} => ${a.toFormInd(n1)}"
            .mkString("\n")}\n${"\t" * n}]:"
      case _ => toFormB(_.toFormInd(n))

  def cmp(t: ANY): Int = (this, t) match
    case (UN, UN)                 => 0
    case (UN, _)                  => -1
    case (_, UN)                  => 1
    case (TF(x), TF(y)) if x == y => 0
    case (TF(x), _)               => if x then 1 else -1
    case (_, _: TF)               => -t.cmp(this)
    case (NCmy(_), _)             => toTF.cmp(t)
    case (_, NCmy(_))             => -t.cmp(this)
    case (Itc(x), _) if !x.toBool => NUM(0).cmp(t)
    case (_, Itc(x)) if !x.toBool => cmp(NUM(0))
    case (Itc(x), _) =>
      val x1 = x.toSEQ.x
      val t1 = t.toSEQ.x
      x1.zip(t1)
        .map { case (a, b) => a.cmp(b) }
        .find(_ != 0)
        .getOrElse(x1.sizeCompare(t1))
    case (_, Itc(_))      => -t.cmp(this)
    case (Sty(x), Sty(y)) => x.compare(y).sign
    case (DBL(x), Nmy(y)) => x.compare(y.toDBL.x)
    case (Nmy(_), _: DBL) => -t.cmp(this)
    case (NUM(x), NUM(y)) => x.compare(y)
    case (NUM(x), _) =>
      x.compare(t.toString.map(_.toInt).applyOrElse(0, _ => 0))
    case (_, _: NUM) => -t.cmp(this)
    case _           => toSTR.cmp(t.toSTR)

  def eql(t: ANY): Boolean  = cmp(t) == 0
  def eqls(t: ANY): Boolean = cmp(t) == 0 && getType == t.getType

  def toBool: Boolean = this match
    case TF(x)                                            => x
    case SEQ(x)                                           => x.nonEmpty
    case ARR(x)                                           => x.nonEmpty
    case MAP(x)                                           => x.nonEmpty
    case STR(x)                                           => x.nonEmpty
    case _: NUM                                           => !eql(NUM(0))
    case _: DBL                                           => !eql(DBL(0))
    case CMD(x)                                           => x.nonEmpty
    case FN(_, _, x)                                      => x.nonEmpty
    case TRY(Failure(_)) | _: ERR | _: TASK | _: OBS | UN => false
    case OSTRAT(OverflowStrategy.Unbounded)               => false
    case TRY(Success(_)) | _: OSTRAT                      => true
    case FUT(x)                                           => x.isCompleted && x.value.get.isSuccess

  def toTF: TF = this match
    case x: TF => x
    case _     => toBool.boolTF

  def toTry: Try[ANY] = this match
    case TRY(x) => x
    case ERR(e) => Failure(e)
    case x      => Success(x)

  def toJSON: ujson.Value = this match
    case MAP(x) => x.map { case (k, v) => (k.toString, v.toJSON) }
    case ARR(x) => x.map(_.toJSON)
    case Lsy(_) => toARR.toJSON
    case NUM(x) => x.toDouble
    case TF(x)  => x
    case UN     => null
    case _      => toString

  def toMs: FiniteDuration = toNUM.x.toLong.millisecond

  def length: Int = this match
    case UN     => 0
    case Lsy(x) => x.length
    case ARR(x) => x.length
    case MAP(x) => x.size
    case Sty(x) => x.length
    case _      => -1

  def olength: Task[Long] = toOBS.x.countL

  def shape: Vector[Int] = this match
    case Itr(x) => x.length +: x.get(NUM(0)).shape
    case _      => Vector()

  def dshape: Vector[Int] = this match
    case Itr(x) =>
      x.length +: x.foldLeft(Vector[Int]()): (a, b) =>
        val b1 = b.dshape
        if a.length < b1.length then b1 else a
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

  def raxes: ANY = paxes(shape.indices.reverse.toVector)

  def paxes(p: Vector[Int]): ANY =
    val s  = shape
    val pg = p.indices.sortBy(p).to(LazyList) #::: LazyList.continually(-1)
    val p1 = s.indices
      .lazyZip(pg)
      .map { case (a, b) => if b < 0 then a else b }
      .to(LazyList)
    val s1 = p1.map(s)
    Util
      .cProd(s1.map(0.until(_).to(LazyList)))
      .map(a => getn(p1.map(i => NUM(a(i))).toSEQ))
      .toSEQ
      .mItr(this)
      .reshape(s1.toVector)

  def get(i: ANY): ANY =
    val oi = i.optI
    this match
      case OBS(x) if oi.isDefined => x.drop(oi.get).headOrElseL(UN).toTASK
      case Its(_) if oi.isDefined =>
        val i1 = oi.get
        val i2 = if i1 < 0 then i1 + length else i1
        this match
          case Lsy(x) => x.applyOrElse(i2, _ => UN)
          case ARR(x) => x.applyOrElse(i2, _ => UN)
          case Sty(x) =>
            if i2 < x.length then x(i2).toString.mSTR(this) else UN
          case _ => toARR.get(NUM(i2))
      case MAP(x)          => x.applyOrElse(i, _ => UN)
      case TRY(Success(x)) => x
      case TRY(Failure(e)) => if i.toBool then ERR(e) else throw e
      case _               => UN

  def gets(is: ANY): ANY = is.map(get)

  def getn(is: ANY): ANY = is.foldLeft(this)(_.get(_))

  def set(i: ANY, t: ANY): ANY =
    val oi = i.optI
    this match
      case OBS(x) if oi.isDefined =>
        val i1 = oi.get
        x.take(i1).:+(t).++(x.drop(i1 + 1)).toOBS
      case Its(_) if oi.isDefined =>
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

  def sets(is: Map[ANY, ANY]): ANY = is.foldLeft(this):
    case (a, (i, t)) => a.set(i, t)

  def setn(is: SEQW[ANY], t: ANY): ANY = is match
    case LazyList()  => this
    case LazyList(i) => set(i, t)
    case i #:: is    => setmod(i, _.setn(is, t))

  def setmod(i: ANY, f: ANY => ANY): ANY = set(i, f(get(i)))

  def setmods(is: Map[ANY, ANY => ANY]): ANY = is.foldLeft(this):
    case (a, (i, f)) => a.setmod(i, f)

  def setmodn(is: SEQW[ANY], f: ANY => ANY): ANY = is match
    case LazyList()  => f(this)
    case LazyList(i) => setmod(i, f)
    case i #:: is    => setmod(i, _.setmodn(is, f))

  def remove(i: ANY): ANY =
    val oi = i.optI
    this match
      case Its(_) if oi.isDefined =>
        val i1 = oi.get
        val i2 = if i1 < 0 then i1 + length else i1
        if i2 >= 0 then
          this match
            case OBS(x) => x.take(i2).++(x.drop(i2 + 1)).toOBS
            case Lsy(x) => x.patch(i2, Nil, 1).mSEQ(this)
            case ARR(x) => x.patch(i2, Nil, 1).toARR
            case _      => toARR.remove(NUM(i2)).matchType(this)
        else this
      case MAP(x) => (x - i).toMAP
      case _      => this

  def add$$(t: ANY): ANY = (this, t) match
    case (UN, y)          => y
    case (x, UN)          => x
    case (OBS(x), OBS(y)) => OBS(x ++ y)
    case (ARR(x), ARR(y)) => ARR(x ++ y)
    case (MAP(x), MAP(y)) => MAP(x ++ y)
    case (Lsy(x), Itr(y)) => SEQ(x #::: y.toSEQ.x).matchType(this)
    case (Itr(x), Lsy(y)) => SEQ(x.toSEQ.x #::: y).matchType(t)
    case (Its(_), _)      => snoc(t)
    case (_, Its(_))      => cons(t)
    case (x, y)           => Vector(x).toARR.add$$(y)

  def cons(t: ANY): ANY = t match
    case OBS(x) => (this +: x).toOBS
    case Lsy(x) => (this #:: x).mSEQ(t)
    case ARR(x) => (this +: x).toARR
    case MAP(x) => (VectorMap(get(NUM(0)) -> get(NUM(1))) ++ x).toMAP
    case Sty(x) => (toString + x).mSTR(t)
    case _      => cons(t.toARR)

  def snoc(t: ANY): ANY = this match
    case OBS(x) => (x :+ t).toOBS
    case Lsy(x) => (x :+ t).mSEQ(this)
    case ARR(x) => (x :+ t).toARR
    case MAP(x) => (x + (t.get(NUM(0)) -> t.get(NUM(1)))).toMAP
    case Sty(x) => (x + t.toString).mSTR(this)
    case _      => toARR.snoc(t)

  def sub$$(t: ANY): ANY = (this, t) match
    case (UN, y) => y
    case (x, UN) => x
    case (OBS(x), OBS(y)) =>
      x.filterEval(a => y.findL(_ == a).map(_.isDefined)).toOBS
    case (OBS(x), y)      => x.filterNot(y.has).toOBS
    case (Lsy(x), Itr(y)) => x.filterNot(y.has).mSEQ(this)
    case (ARR(x), Itr(y)) => x.filterNot(y.has).toARR
    case (MAP(x), MAP(y)) => y.foldLeft(x)(_ - _._1).toMAP
    case (MAP(x), It(y))  => y.foldLeft(x)(_ - _).toMAP
    case (Itr(x), y)      => x.sub$$(Vector(y).toARR)
    case (x, y)           => Vector(x).toARR.sub$$(y)

  def mul$$(t: ANY): ANY = (this, t) match
    case (Itr(x), Itr(y)) => x.zip(y)(_ mul$$ _).flat
    case (Lsy(_), y)      => LazyList.fill(y.toInt)(this).toSEQ.flat.matchType(this)
    case (x: ARR, y)      => Vector.fill(y.toInt)(x).toARR.flat
    case (Sty(_), y)      => toARR.mul$$(y).toString.mSTR(this)
    case (x, y)           => Vector(x).toARR.mul$$(y)

  def div$$(n: Int): ANY = this match
    case OBS(x) => x.bufferTumbling(n).map(_.toARR).toOBS
    case Lsy(x) => x.grouped(n).map(_.toSEQ).mSEQ(this)
    case ARR(x) => x.grouped(n).map(_.toARR).toSEQ
    case MAP(x) => x.grouped(n).map(_.toMAP).toSEQ
    case Sty(_) => toARR.div$$(n).map(_.toString.mSTR(this)).toSEQ
    case x      => Vector(x).toARR.div$$(n)

  def mod$$(n: Int, k: Int = 1): ANY = this match
    case OBS(x) => x.bufferSliding(n, k).map(_.toARR).toOBS
    case Lsy(x) => x.sliding(n, k).map(_.toSEQ).mSEQ(this)
    case ARR(x) => x.sliding(n, k).map(_.toARR).toSEQ
    case MAP(x) => x.sliding(n, k).map(_.toMAP).toSEQ
    case Sty(_) => toARR.mod$$(n, k).map(_.toString.mSTR(this)).toSEQ
    case x      => Vector(x).toARR.mod$$(n, k)

  def take(n: Int): ANY =
    if n < 0 && length != 0 then drop(length + n)
    else
      this match
        case OBS(x) => x.take(n).toOBS
        case Lsy(x) => x.take(n).mSEQ(this)
        case ARR(x) => ARR(x.take(n))
        case MAP(x) => MAP(x.take(n))
        case Sty(x) => x.take(n).mSTR(this)
        case _      => toARR.take(n)

  def drop(n: Int): ANY =
    if n < 0 && length != 0 then take(length + n)
    else
      this match
        case OBS(x) => x.drop(n).toOBS
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
    case Lsy(x) => x.combo(n).map(_.mSEQ(this)).toSEQ
    case ARR(x) => combHelper(x, _.combinations(n)).map(_.toARR).toSEQ
    case _      => toARR.combinations(n).map(_.matchType(this))

  def powset: ANY = this match
    case SEQ(x) =>
      val x1 = x.zipWithIndex.map(_._2)
      x1.flatMap(a => x1.combo(a + 1))
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
    case _: OBS => toTASK.x.runSyncUnsafe(Duration.Inf).toARR
    case x: ARR => x
    case Lsy(x) => x.toARR
    case MAP(x) => x.toVector.map { case (i, a) => Vector(i, a).toARR }.toARR
    case Sty(x) => x.toVector.map(c => c.toString.mSTR(this)).toARR
    case _      => Vector(this).toARR

  def toMAP: MAP = this match
    case x: MAP => x
    case ARR(x) =>
      x.flatMap:
        case Itr(a) if a.length > 0 => Some((a.get(NUM(0)), a.get(NUM(1))))
        case _                      => None
      .to(VectorMap)
        .toMAP
    case _ => toARR.toMAP

  def toSTR: STR = STR(toString)

  def toNUM: NUM =
    try
      this match
        case x: NUM => x
        case UN     => NUM(0)
        case TF(x)  => x.boolNUM
        case Sty(x) => x.toNUM
        case _      => toString.toNUM
    catch
      case _: java.lang.NumberFormatException =>
        throw LinEx("CAST", "bad NUM cast " + toForm)
      case e => throw e

  def toDBL: DBL = this match
    case x: DBL => x
    case UN     => DBL(0)
    case _      => toNUM.x.toDouble.toDBL

  def toERR: ERR = this match
    case Ery(x) => ERR(x)
    case x      => LinEx("_", x.toString).pipe(ERR(_))

  def toTRY: TRY = this match
    case x: TRY => x
    case _      => toTry.toTRY

  def optNUM: Option[NUM] = Try(toNUM).toOption

  def toInt: Int = toNUM.x.intValue

  def optI: Option[Int] = optNUM.map(_.x.intValue)

  def xFN: LazyList[ANY] = this match
    case Lsy(x) => x
    case Sty(x) => Parser.parse(x)
    case _      => toSEQ.xFN

  def toFN(env: ENV): FN = env.code.copy(x = xFN)

  def lFN(l: Int, env: ENV): FN = FN(PATH(env.code.p.f, l), env.code.s, xFN)

  def pFN(p: PATH, s: SCOPE): FN = FN(p, s, xFN)

  def toTASK: TASK = this match
    case x: TASK => x
    case OBS(x)  => x.toListL.map(_.toARR).toTASK
    case FUT(x)  => Task.fromFuture(x).toTASK
    case ERR(x)  => Task.raiseError(x).toTASK
    case x       => Task.pure(x).toTASK

  def modTASK(f: Task[ANY] => Task[ANY]): TASK =
    toTASK.x.pipe(f).toTASK

  def modOBS(f: Observable[ANY] => Observable[ANY]): OBS =
    toOBS.x.pipe(f).toOBS

  def toFUT: FUT = this match
    case x: FUT  => x
    case TASK(x) => x.runToFuture.toFUT
    case _: OBS  => toTASK.toFUT
    case ERR(x)  => CancelableFuture.failed(x).toFUT
    case x       => CancelableFuture.pure(x).toFUT

  def toOBS: OBS = this match
    case x: OBS          => x
    case Lsy(_) | Itr(_) => Observable.fromIterable(toSEQ.x).toOBS
    case FUT(x)          => Observable.fromFuture(x).toOBS
    case TASK(x)         => Observable.fromTask(x).toOBS
    case TRY(x)          => Observable.fromTry(x).toOBS
    case ERR(x)          => Observable.raiseError(x).toOBS
    case UN              => Observable.empty.toOBS
    case x               => Observable.pure(x).toOBS

  def toOSTRAT: OSTRAT = this match
    case x: OSTRAT => x
    case Nmy(x)    => OSTRAT(OverflowStrategy.BackPressure(x.toInt))
    case _         => OSTRAT(OverflowStrategy.Unbounded)

  def matchType(a: ANY): ANY = a match
    case FN(p, s, _) => pFN(p, s)
    case _: SEQ      => toSEQ
    case _: ARR      => toARR
    case _: MAP      => toMAP
    case _: STR      => toSTR
    case _: NUM      => toNUM
    case _: DBL      => toDBL
    case _: TF       => toTF
    case _: CMD      => toString.pipe(CMD(_))
    case _: FUT      => toFUT
    case _: TASK     => toTASK
    case _: OBS      => toOBS
    case _: OSTRAT   => toOSTRAT
    case _: TRY      => toTRY
    case _: ERR      => toERR
    case UN          => UN

  def toItr: ANY = this match
    case Lsy(_) | Itr(_) => this
    case _               => toARR

  def mItr(a: ANY): ANY = a match
    case Lsy(_) | Itr(_) => matchType(a)
    case _               => toARR

  def mIts(a: ANY): ANY = a match
    case Its(_) => matchType(a)
    case _      => toARR

  def toFile: File = this match
    case Itr(_) => foldLeft(File(""))((a, b) => a / b.toString)
    case _      => File(toString)

  def map(f: ANY => ANY): ANY = this match
    case OBS(x)  => x.map(f).toOBS
    case TASK(x) => x.map(f).toTASK
    case FUT(x)  => x.map(f).toFUT
    case Lsy(x)  => x.map(f).mSEQ(this)
    case ARR(x)  => x.map(f).toARR
    case TRY(x)  => x.map(f).toTRY
    case _       => toARR.map(f)
  def map(f: (ANY, ANY) => (ANY, ANY), g: ANY => ANY): ANY = this match
    case MAP(x) => x.map(f.tupled).toMAP
    case _      => map(g)
  def mapEval(f: ANY => ANY): OBS = modOBS(_.mapEval(f(_).toTASK.x))

  def flatMap(f: ANY => ANY): ANY = this match
    case OBS(x)  => x.flatMap(f(_).toOBS.x).toOBS
    case TASK(x) => x.flatMap(f(_).toTASK.x).toTASK
    case FUT(x)  => x.flatMap(f(_).toFUT.x).toFUT
    case Lsy(x)  => x.flatMap(f(_).toSEQ.x).mSEQ(this)
    case ARR(x)  => x.flatMap(f(_).toARR.x).toARR
    case TRY(x)  => x.flatMap(f(_).toTry).toTRY
    case _       => toARR.flatMap(f)
  def flatMap(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) => x.flatMap { case (a, b) => f(a, b).toARR.x }.toARR
    case _      => flatMap(g)
  def flatMap$(f: ANY => ARRW[ANY]): ANY = this match
    case Lsy(x) => x.flatMap(f).mSEQ(this)
    case ARR(x) => x.flatMap(f).toARR
    case _      => toARR.flatMap$(f)
  def flatMap$(f: (ANY, ANY) => ARRW[ANY], g: ANY => ARRW[ANY]): ANY =
    this match
      case MAP(x) =>
        x.flatMap:
          case (a, b) =>
            f(a, b) match
              case _ :+ k :+ v => Some(k, v)
              case Vector(v)   => Some(a, v)
              case _           => None
        .toMAP
      case _ => flatMap$(g)
  def winMap(n: Int, f: ARRW[ANY] => ANY): ANY =
    mod$$(n).map(_.toARR.x.pipe(f)).mIts(this)
  def winMapM(n: Int, f: ARRW[ANY] => (ANY, ANY), g: ARRW[ANY] => ANY): ANY =
    this match
      case _: MAP =>
        mod$$(n)
          .map:
            case MAP(x) =>
              x.keys.++(x.values).pipe(f) match
                case (k, v) => Vector(k, v).toARR
          .toMAP
      case _ => winMap(n, g)
  def mergeMap(f: ANY => ANY): OBS  = modOBS(_.mergeMap(f(_).toOBS.x))
  def switchMap(f: ANY => ANY): OBS = modOBS(_.switchMap(f(_).toOBS.x))
  def flat: ANY                     = flatMap(x => x)
  def merge: OBS                    = mergeMap(x => x)
  def rflat: ANY = this match
    case Itr(_) => flatMap(_.rflat)
    case x      => x

  def foreach(f: ANY => Unit): ANY = this match
    case OBS(x) => x.foreach(f).map(_ => UN).toFUT
    case _ =>
      this match
        case TASK(x) => x.foreach(f)
        case Lsy(x)  => x.foreach(f)
        case ARR(x)  => x.foreach(f)
        case TRY(x)  => x.foreach(f)
        case _       => toARR.foreach(f)
      UN
  def foreach(f: (ANY, ANY) => Unit, g: ANY => Unit): ANY = this match
    case MAP(x) =>
      x.foreach(f.tupled)
      UN
    case _ => foreach(g)

  def zip(t: ANY)(f: (ANY, ANY) => ANY): ANY = (this, t) match
    case (OBS(x), _)  => x.zipMap(t.toOBS.x)(f).toOBS
    case (_, _: OBS)  => toOBS.zip(t)(f).toOBS
    case (TASK(x), _) => Task.map2(x, t.toTASK.x)(f).toTASK
    case (_, _: TASK) => toTASK.zip(t)(f).toTASK
    case (FUT(x), _)  => x.zip(t.toFUT.x).map(f.tupled).toFUT
    case (_, _: FUT)  => toFUT.zip(t)(f).toFUT
    case (Lsy(x), _)  => x.zip(t.toSEQ.x).map(f.tupled).mSEQ(this)
    case (_, Lsy(_))  => toSEQ.zip(t)(f).mSEQ(t)
    case _            => toARR.x.lazyZip(t.toARR.x).map(f).toARR

  def combine(t: ANY, f: (ANY, ANY) => ANY): OBS = modOBS:
    _.combineLatestMap(t.toOBS.x)(f)

  def zipAll(t: ANY, d1: ANY, d2: ANY, f: (ANY, ANY) => ANY): ANY =
    (this, t) match
      case (_: OBS, _)  => combine(t, f).toOBS
      case (_, _: OBS)  => toOBS.zipAll(t, d1, d2, f).toOBS
      case (_, _: TASK) => toTASK.zip(t)(f)
      case (_: TASK, _) => zip(t.toTASK)(f)
      case (_, _: FUT)  => toFUT.zip(t)(f)
      case (_: FUT, _)  => zip(t.toFUT)(f)
      case (Lsy(x), _)  => x.zipAll(t.toSEQ.x, d1, d2).map(f.tupled).mSEQ(this)
      case (_, Lsy(_))  => toSEQ.zipAll(t, d1, d2, f).mSEQ(t)
      case _            => toARR.x.zipAll(t.toARR.x, d1, d2).map(f.tupled).toARR

  def table(t: ANY, f: (ANY, ANY) => ANY): ANY = map(x => t.map(y => f(x, y)))
  def flatTable(t: ANY, f: (ANY, ANY) => ANY): ANY =
    flatMap(x => t.map(y => f(x, y)))

  def rmap(f: ANY => ANY): ANY = this match
    case Itr(_) => map((k, v) => (k, v.rmap(f)), _.rmap(f))
    case x      => f(x)

  def dmap(d: Int, f: ANY => ANY): ANY =
    if d == 0 then rmap(f)
    else
      def loop(i: Int, t: ANY): ANY = t match
        case Itr(_) if i > 0 =>
          t.map((k, v) => (k, loop(i - 1, v)), loop(i - 1, _))
        case Itr(_) if i < 0 && -i < t.depth =>
          t.map((k, v) => (k, loop(i, v)), loop(i, _))
        case _ => f(t)
      loop(d, this)

  def toShape(t: ANY): ANY =
    val q = toSEQ.rflat.toSEQ.x
    var f = LazyList.continually(q).flatten
    t.rmap: _ =>
      f match
        case x #:: xs =>
          f = xs
          x
        case _ => UN

  def foldLeft[T](a: T)(f: (T, ANY) => T): T = this match
    case Lsy(x) => x.foldLeft(a)(f)
    case ARR(x) => x.foldLeft(a)(f)
    case _      => toARR.foldLeft(a)(f)
  def foldLeft[T](a: T)(f: (T, (ANY, ANY)) => T, g: (T, ANY) => T): T =
    this match
      case MAP(x) => x.foldLeft(a)((b, c) => f(b, c))
      case _      => foldLeft(a)(g)
  def ofoldLeft[T](a: T)(f: (T, ANY) => T): Task[T] = toOBS.x.foldLeftL(a)(f)

  def foldRight[T](a: T)(f: (ANY, T) => T): T = this match
    case Lsy(x) => x.foldRight(a)(f)
    case ARR(x) => x.foldRight(a)(f)
    case _      => toARR.foldRight(a)(f)
  def foldRight[T](a: T)(f: ((ANY, ANY), T) => T, g: (ANY, T) => T): T =
    this match
      case MAP(x) => x.foldRight(a)(f)
      case _      => foldRight(a)(g)

  def rfoldLeft[T](a: T)(f: (T, ANY) => T): T =
    this match
      case Itr(_) =>
        foldLeft(a)(
          { case (s, (_, v)) => v.rfoldLeft(s)(f) },
          (s, v) => v.rfoldLeft(s)(f)
        )
      case x => f(a, x)

  def rfoldRight[T](a: T)(f: (T, ANY) => T): T =
    this match
      case Itr(_) =>
        foldRight(a)(
          { case ((_, v), s) => v.rfoldRight(s)(f) },
          (v, s) => v.rfoldRight(s)(f)
        )
      case x => f(a, x)

  def reduceH(f: => ANY): ANY = try f
  catch
    case _: java.lang.UnsupportedOperationException =>
      throw LinEx("ITR", s"unable to reduce empty $getType")
  def reduceMH(f: (ANY, (ANY, ANY)) => ANY)(t: ANY, s: ANY): ANY = s match
    case ARR(k +: v +: _) => f(t, (k, v))

  def reduceLeft(f: (ANY, ANY) => ANY): ANY = reduceH:
    this match
      case Lsy(x) => x.reduceLeft(f)
      case ARR(x) => x.reduceLeft(f)
      case _      => toARR.reduceLeft(f)
  def reduceLeft(f: (ANY, (ANY, ANY)) => ANY, g: (ANY, ANY) => ANY): ANY =
    this match
      case _: MAP =>
        toARR.reduceLeft(reduceMH(f))
      case _ => reduceLeft(g)

  def reduceRight(f: (ANY, ANY) => ANY): ANY =
    reduceH:
      this match
        case Lsy(x) => x.reduceRight(f).matchType(this)
        case ARR(x) => x.reduceRight(f)
        case _      => toARR.reduceRight(f)
  def reduceRight(f: (ANY, (ANY, ANY)) => ANY, g: (ANY, ANY) => ANY): ANY =
    this match
      case _: MAP =>
        toARR.reduceRight(reduceMH(f))
      case _ => reduceRight(g)

  def scanLeft(a: ANY)(f: (ANY, ANY) => ANY): ANY = this match
    case OBS(x) => x.scan0(a)(f).toOBS
    case Lsy(x) => x.scanLeft(a)(f).mSEQ(this)
    case ARR(x) => x.scanLeft(a)(f).toARR
    case _      => toARR.scanLeft(a)(f)
  def scanLeft(
      a: ANY
  )(f: (ANY, (ANY, ANY)) => ANY, g: (ANY, ANY) => ANY): ANY = this match
    case MAP(x) => x.scanLeft(a)(f).toARR
    case _      => scanLeft(a)(g)
  def scanEval(a: ANY)(f: (ANY, ANY) => ANY): OBS = modOBS:
    _.scanEval0(a.toTASK.x)(f(_, _).toTASK.x)

  def scanRight(a: ANY)(f: (ANY, ANY) => ANY): ANY = this match
    case Lsy(x) => x.scanRight(a)(f).mSEQ(this)
    case ARR(x) => x.scanRight(a)(f).toARR
    case _      => toARR.scanRight(a)(f)
  def scanRight(
      a: ANY
  )(f: ((ANY, ANY), ANY) => ANY, g: (ANY, ANY) => ANY): ANY = this match
    case MAP(x) => x.scanRight(a)(f).toARR
    case _      => scanRight(a)(g)

  def filter(f: ANY => Boolean): ANY = this match
    case OBS(x) => x.filter(f).toOBS
    case Lsy(x) => x.filter(f).mSEQ(this)
    case ARR(x) => x.filter(f).toARR
    case x: TRY => x.toTry.filter(f).toTRY
    case _      => toARR.filter(f)
  def filter(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.filter(f.tupled).toMAP
    case _      => filter(g)
  def filterEval(f: ANY => ANY): OBS = modOBS:
    _.filterEval(f(_).toTASK.x.map(_.toBool))

  def any(f: ANY => Boolean): Boolean = this match
    case Lsy(x) => x.exists(f)
    case ARR(x) => x.exists(f)
    case _      => toARR.any(f)
  def any(f: (ANY, ANY) => Boolean, g: ANY => Boolean): Boolean = this match
    case MAP(x) => x.exists(f.tupled)
    case _      => any(g)
  def oany(f: ANY => Boolean): Task[Boolean] = toOBS.x.existsL(f)

  def all(f: ANY => Boolean): Boolean = this match
    case Lsy(x) => x.forall(f)
    case ARR(x) => x.forall(f)
    case _      => toARR.all(f)
  def all(f: (ANY, ANY) => Boolean, g: ANY => Boolean): Boolean = this match
    case MAP(x) => x.forall(f.tupled)
    case _      => all(g)
  def oall(f: ANY => Boolean): Task[Boolean] = toOBS.x.forallL(f)

  def takeWhile(f: ANY => Boolean): ANY = this match
    case OBS(x) => x.takeWhile(f).toOBS
    case Lsy(x) => x.takeWhile(f).mSEQ(this)
    case ARR(x) => x.takeWhile(f).toARR
    case _      => toARR.takeWhile(f)
  def takeWhile(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.takeWhile(f.tupled).toMAP
    case _      => takeWhile(g)

  def dropWhile(f: ANY => Boolean): ANY = this match
    case OBS(x) => x.dropWhile(f).toOBS
    case Lsy(x) => x.dropWhile(f).mSEQ(this)
    case ARR(x) => x.dropWhile(f).toARR
    case _      => toARR.dropWhile(f)
  def dropWhile(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.dropWhile(f.tupled).toMAP
    case _      => dropWhile(g)

  def find(f: ANY => Boolean): Option[ANY] = this match
    case Lsy(x) => x.find(f)
    case ARR(x) => x.find(f)
    case _      => toARR.find(f)
  def find(
      f: (ANY, ANY) => Boolean,
      g: ANY => Boolean
  ): Option[ANY | (ANY, ANY)] = this match
    case MAP(x) => x.find(f.tupled)
    case _      => find(g)
  def ofind(f: ANY => Boolean): Task[Option[ANY]] = toOBS.x.findL(f)

  def findIndex(f: ANY => Boolean): Int = this match
    case Lsy(x) => x.indexWhere(f)
    case ARR(x) => x.indexWhere(f)
    case _      => toARR.findIndex(f)

  def delBy(f: ANY => Boolean): ANY = this match
    case Lsy(x) => x.delBy(f).mSEQ(this)
    case ARR(x) => x.delBy(f).toARR
    case _      => toARR.delBy(f)
  def delBy(
      f: (ANY, ANY) => Boolean,
      g: ANY => Boolean
  ): ANY = this match
    case MAP(x) => x.delBy(f.tupled).to(VectorMap).toMAP
    case _      => delBy(g)

  def uniqBy(f: ANY => ANY): ANY = this match
    case OBS(x) => x.distinctUntilChangedByKey(f).toOBS
    case Lsy(x) => x.distinctBy(f).mSEQ(this)
    case ARR(x) => x.distinctBy(f).toARR
    case _      => toARR.uniqBy(f)
  def uniqBy(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) =>
      x.toSeq.distinctBy(f.tupled).to(VectorMap).toMAP
    case _ => uniqBy(g)

  def uniqWith(f: (ANY, ANY) => Boolean): ANY = this match
    case Lsy(x) => x.uniqWith(f).mSEQ(this)
    case ARR(x) => x.uniqWith(f).toARR
    case _      => toARR.uniqWith(f)
  def uniqWith(
      f: ((ANY, ANY), (ANY, ANY)) => Boolean,
      g: (ANY, ANY) => Boolean
  ): ANY = this match
    case MAP(x) => x.toSeq.uniqWith(f).to(VectorMap).toMAP
    case _      => uniqWith(g)

  def sortBy(f: ANY => ANY): ANY = this match
    case Lsy(x) => x.sortBy(f)(OrdANY).mSEQ(this)
    case ARR(x) => x.sortBy(f)(OrdANY).toARR
    case _      => toARR.sortBy(f)
  def sortBy(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) =>
      x.toSeq.sortBy(f.tupled)(OrdANY).to(VectorMap).toMAP
    case _ => sortBy(g)

  def sortWith(f: (ANY, ANY) => Boolean): ANY = this match
    case Lsy(x) => x.sortWith(f).mSEQ(this)
    case ARR(x) => x.sortWith(f).toARR
    case _      => toARR.sortWith(f)
  def sortWith(
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
  def partition(f: (ANY, ANY) => Boolean, g: ANY => Boolean): (ANY, ANY) =
    this match
      case MAP(x) =>
        x.partition(f.tupled).pipe { case (a, b) => (a.toMAP, b.toMAP) }
      case _ => partition(g)

  def groupBy(f: ANY => ANY): Map[ANY, ANY] = this match
    case Lsy(x) => x.groupBy(f).view.mapValues(_.mSEQ(this)).toMap
    case ARR(x) => x.groupBy(f).view.mapValues(_.toARR).toMap
    case _      => toARR.groupBy(f)
  def groupBy(f: (ANY, ANY) => ANY, g: ANY => ANY): Map[ANY, ANY] = this match
    case MAP(x) =>
      x.groupBy(f.tupled).view.mapValues(_.toMAP).toMap
    case _ => groupBy(g)
  def ogroupBy(f: ANY => ANY): OBS = modOBS:
    _.groupBy(f).map(a => Vector(a.key, a.toOBS).toARR)

  def span(f: ANY => Boolean): (ANY, ANY) = this match
    case Lsy(x) =>
      x.span(f).pipe { case (a, b) => (a.mSEQ(this), b.mSEQ(this)) }
    case ARR(x) => x.span(f).pipe { case (a, b) => (a.toARR, b.toARR) }
    case _      => toARR.span(f)
  def span(f: (ANY, ANY) => Boolean, g: ANY => Boolean): (ANY, ANY) =
    this match
      case MAP(x) => x.span(f.tupled).pipe { case (a, b) => (a.toMAP, b.toMAP) }
      case _      => span(g)

  def packWith(f: (ANY, ANY) => Boolean): ANY = this match
    case Lsy(x) => x.packWith(f).map(_.mSEQ(this)).mSEQ(this)
    case ARR(x) => x.packWith(f).map(_.toARR).toARR
    case _      => toARR.packWith(f)
  def packWith(
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

  def vec2(t: ANY)(f: (ANY, ANY) => ANY): ANY = (this, t) match
    case (Itr(_), Itr(_)) => zip(t)(_.vec2(_)(f))
    case (Itr(_), _)      => vec1(f(_, t))
    case (_, Itr(_))      => t.vec1(f(this, _))
    case _                => f(this, t)

  def vec3(t: ANY, s: ANY)(f: (ANY, ANY, ANY) => ANY): ANY = (this, t, s) match
    case (Itr(_), Itr(_), Itr(_)) =>
      zip(t)(Vector(_, _).toARR).zip(s): (x, c) =>
        x.toARR.x match
          case Vector(a, b) => a.vec3(b, c)(f)
    case (Itr(_), Itr(_), _) => vec2(t)(f(_, _, s))
    case (Itr(_), _, Itr(_)) => vec2(t)(f(_, t, _))
    case (_, Itr(_), Itr(_)) => t.vec2(s)(f(this, _, _))
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
    vec2(t)(_.fNum2(_, f(_, _).toDBL, g(_, _).toNUM))
  def num2(
      t: ANY,
      f: (Double, Double) => Double,
      g: (NUMF, NUMF) => NUMF,
      e: String
  ): ANY =
    try num2(t, f, g)
    catch case _: ArithmeticException => throw LinEx("MATH", e)

  def num2q(t: ANY, f: (NUMF, NUMF) => Iterable[NUMF]): ANY =
    vec2(t)((x, y) => f(x.toNUM.x, y.toNUM.x).map(NUM(_)).toSEQ)

  def num2a(t: ANY, f: (NUMF, NUMF) => Iterable[NUMF]): ANY =
    vec2(t)((x, y) => f(x.toNUM.x, y.toNUM.x).map(NUM(_)).toARR)

  def str1(f: String => String): ANY = vec1(_.toString.pipe(f).sSTR)

  def str1a(f: String => Iterable[String]): ANY = vec1:
    _.toString.pipe(f).map(STR(_)).toARR

  def str2(t: ANY, f: (String, String) => String): ANY =
    vec2(t)((x, y) => f(x.toString, y.toString).sSTR)

  def str2q(t: ANY, f: (String, String) => Iterable[String]): ANY =
    vec2(t)((x, y) => f(x.toString, y.toString).map(_.sSTR).toSEQ)

  def str2a(t: ANY, f: (String, String) => Iterable[String]): ANY =
    vec2(t)((x, y) => f(x.toString, y.toString).map(_.sSTR).toARR)

  def strnum(t: ANY, f: (String, NUMF) => String): ANY =
    vec2(t)((x, y) => STR(f(x.toString, y.toNUM.x)))

  def strnumq(t: ANY, f: (String, NUMF) => Iterable[String]): ANY =
    vec2(t)((x, y) => f(x.toString, y.toNUM.x).map(_.sSTR).toSEQ)

  def strnuma(t: ANY, f: (String, NUMF) => Iterable[String]): ANY =
    vec2(t)((x, y) => f(x.toString, y.toNUM.x).map(_.sSTR).toARR)

  def fNum1(f: Double => ANY, g: NUMF => ANY): ANY = this match
    case DBL(x) => f(x)
    case _      => g(toNUM.x)

  def fNum2(t: ANY, f: (Double, Double) => ANY, g: (NUMF, NUMF) => ANY): ANY =
    (this, t) match
      case (DBL(x), Nmy(y)) => f(x, y.toDBL.x)
      case (Nmy(x), DBL(y)) => f(x.toDBL.x, y)
      case (x, y)           => g(x.toNUM.x, y.toNUM.x)

object ANY:

  val wholes: LazyList[NUM] = LazyList.iterate(0: Real)(_ + 1).map(NUM.apply)

  def exitCase(e: ExitCase[Throwable]): ANY = e match
    case ExitCase.Completed => TF(true)
    case ExitCase.Canceled  => TF(false)
    case ExitCase.Error(e)  => ERR(e)

  /** Pattern for `SEQ`-like. */
  object Itr:

    def unapply(a: ANY): Option[ANY] = a match
      case _: OBS | _: SEQ | _: ARR | _: MAP => Some(a)
      case _                                 => None

  object Fnr:

    def unapply(a: ANY): Option[ANY] = a match
      case Itr(_) | _: TASK | _: FUT | _: TRY => Some(a)
      case _                                  => None

  /** Pattern for strict `SEQ`-like. */
  object It:

    def unapply(a: ANY): Option[ANY] = a match
      case _: SEQ | _: ARR => Some(a)
      case _               => None

  /** Pattern for loose `SEQ`-like. */
  object Its:

    def unapply(a: ANY): Option[ANY] = a match
      case _: OBS | Lsy(_) | _: ARR | Sty(_) => Some(a)
      case _                                 => None

  /** Pattern for comparable `SEQ`-like. */
  object Itc:

    def unapply(a: ANY): Option[ANY] = a match
      case Lsy(_) | _: ARR | _: MAP => Some(a)
      case _                        => None

  object NCmy:

    def unapply(a: ANY): Option[ANY] = a match
      case Itc(_) | Nmy(_) | Sty(_) | _: TF | UN => None
      case _                                     => Some(a)

  object Lsy:

    def unapply(a: ANY): Option[LazyList[ANY]] = a match
      case SEQ(x)      => Some(x)
      case FN(_, _, x) => Some(x)
      case _           => None

  object Nmy:

    def unapply(a: ANY): Option[ANY] = a match
      case _: NUM | _: DBL => Some(a)
      case _               => None

  object Sty:

    def unapply(a: ANY): Option[String] = a match
      case STR(x) => Some(x)
      case CMD(x) => Some(x)
      case _      => None

  object Ery:

    def unapply(a: ANY): Option[Throwable] = a match
      case ERR(x)          => Some(x)
      case TRY(Failure(x)) => Some(x)
      case _               => None

  extension [T](xs: Iterable[T])

    def packWith(f: (T, T) => Boolean): Iterable[Iterable[T]] =
      @tailrec def loop(
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
      xs.foldLeft(Iterable[T]()): (acc, x) =>
        if acc.exists(f(x, _)) then acc else acc ++ Iterable(x)

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

    def combo(n: Int): SEQW[SEQW[T]] =
      if n < 0 then LazyList()
      else if n == 0 then LazyList(LazyList())
      else
        xs match
          case LazyList() => LazyList()
          case x #:: xs   => xs.combo(n - 1).map(x #:: _) #::: xs.combo(n)

  extension (x: Iterable[ANY])

    def toSEQ: SEQ = SEQ(x.to(LazyList))
    def mSEQ(t: ANY): ANY = t match
      case Lsy(_) => toSEQ.matchType(t)
      case _      => toSEQ
    def toARR: ARR                 = ARR(x.toVector)
    def pFN(p: PATH, s: SCOPE): FN = FN(p, s, x.to(LazyList))

  extension (x: Iterator[ANY])

    def toSEQ: SEQ = SEQ(x.to(LazyList))
    def mSEQ(t: ANY): ANY = t match
      case Lsy(_) => toSEQ.matchType(t)
      case _      => toSEQ
    def pFN(p: PATH, s: SCOPE): FN = FN(p, s, x.to(LazyList))

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

  extension (t: Try[ANY]) def toTRY: TRY = TRY(t)

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
          m.subgroups.zipWithIndex
            .map:
              case (g, i) =>
                val i1 = i + 1
                VectorMap(
                  STR("&") -> g.strun,
                  STR("`") -> m.before(i1).pipe(strun),
                  STR("'") -> m.after(i1).pipe(strun),
                  STR("^") -> NUM(m.start(i1)),
                  STR("$") -> NUM(m.end(i1))
                ).toMAP
            .toARR,
        STR("^") -> NUM(m.start),
        STR("$") -> NUM(m.end)
      ).toMAP

  extension (n: Double) def toDBL: DBL = DBL(n)

  extension (n: Real) def toNUM: NUM = NUM(n)

  extension [T](o: Observable[T]) def toL: List[T] = o.toListL.runSyncUnsafe()

  extension (t: Task[ANY]) def toTASK: TASK = TASK(t)

  extension (t: FUTW[ANY]) def toFUT: FUT = FUT(t)

  extension (t: Observable[ANY]) def toOBS: OBS = OBS(t)

given ReadWriter[ANY] = readwriter[ujson.Value].bimap[ANY](_.toJSON, _.toANY)

given Eq[ANY] with

  def eqv(x: ANY, y: ANY): Boolean = x.eqls(y)

object OrdANY extends Ordering[ANY]:

  def compare(x: ANY, y: ANY): Int = x.cmp(y)
