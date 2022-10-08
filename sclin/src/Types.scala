import pprint.Tree.Lazy
import scala.collection.immutable.VectorMap
import scala.util.Random
import spire.math._
import util.chaining._
import ANY._

/** ADT for lin types. */
enum ANY:

  case SEQ(x: SEQW[ANY])
  case ARR(x: ARRW[ANY])
  case MAP(x: MAPW[ANY, ANY])
  case STR(x: String)
  case NUM(x: NUMF)
  case CMD(x: String)
  case FN(p: PATH, x: List[ANY])
  case ERR(x: LinERR)
  case UN

  def getType: String = getClass.getSimpleName

  override def toString: String = this match
    case MAP(x) =>
      x.toSeq.map { case (i, a) => i.toString + " " + a.toString }
        .mkString("\n")
    case STR(x) => x
    case NUM(x) => x.toString
    case CMD(x) => x
    case ERR(x) => x.toString
    case UN     => ""
    case _      => join(" ")

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
      s"(${x.map(_.toForm).mkString(" ")})$n"
    case ERR(x) => s"ERR(${x.t})"
    case UN     => "UN"
    case _      => toString

  def cmp(t: ANY): Int = (this, t) match
    case (SEQ(x), SEQ(y)) => x.sizeCompare(y)
    case (SEQ(x), Itr(y)) => x.sizeCompare(y.length)
    case (Itr(x), SEQ(y)) => -y.sizeCompare(x.length)
    case (SEQ(x), NUM(y)) => x.sizeCompare(y.intValue)
    case (NUM(x), SEQ(y)) => -y.sizeCompare(x.intValue)
    case (Itr(x), Itr(y)) => x.length.compare(y.length)
    case (NUM(x), NUM(y)) => x.compare(y)
    case (STR(x), STR(y)) => x.compareTo(y)
    case (NUM(x), STR(y)) => x.compare(y.codePointAt(0))
    case (_: STR, _: NUM) => -t.cmp(this)
    case _                => ???

  def eql(t: ANY): Boolean = (this, t) match
    case (_: NUM, _: NUM) => cmp(t) == 0
    case _                => this == t

  def toBool: Boolean = this match
    case SEQ(x)   => !x.isEmpty
    case ARR(x)   => !x.isEmpty
    case MAP(x)   => !x.isEmpty
    case STR(x)   => !x.isEmpty
    case NUM(_)   => !eql(NUM(0))
    case CMD(x)   => !x.isEmpty
    case FN(_, x) => !x.isEmpty
    case _: ERR   => false
    case UN       => false

  def length: Int = this match
    case SEQ(x)   => x.length
    case ARR(x)   => x.length
    case MAP(x)   => x.size
    case STR(x)   => x.length
    case FN(_, x) => x.length
    case _        => 0

  def get(i: ANY): ANY =
    val oi = i.optI
    this match
      case Its(x) if oi != None =>
        val i1 = oi.get
        val i2 = if i1 < 0 then i1 + length else i1
        this match
          case SEQ(x) => x.applyOrElse(i2, _ => UN)
          case ARR(x) => x.applyOrElse(i2, _ => UN)
          case STR(x) => if i2 < x.length then STR(x(i2).toString) else UN
          case _      => toSEQ.get(NUM(i2))
      case _ =>
        this match
          case MAP(x) => x.applyOrElse(i, _ => UN)
          case _: CMD => toSTR.get(i)
          case _      => UN

  def take(n: Int): ANY = this match
    case SEQ(x)   => SEQ(if n < 0 then x.takeRight(-n) else x.take(n))
    case ARR(x)   => ARR(if n < 0 then x.takeRight(-n) else x.take(n))
    case MAP(x)   => MAP(if n < 0 then x.takeRight(-n) else x.take(n))
    case STR(x)   => STR(if n < 0 then x.takeRight(-n) else x.take(n))
    case FN(p, x) => FN(p, if n < 0 then x.takeRight(-n) else x.take(n))
    case _        => toSEQ.take(n)

  def drop(n: Int): ANY = this match
    case SEQ(x)   => SEQ(if n < 0 then x.dropRight(-n) else x.drop(n))
    case ARR(x)   => ARR(if n < 0 then x.dropRight(-n) else x.drop(n))
    case MAP(x)   => MAP(if n < 0 then x.dropRight(-n) else x.drop(n))
    case STR(x)   => STR(if n < 0 then x.dropRight(-n) else x.drop(n))
    case FN(p, x) => FN(p, if n < 0 then x.dropRight(-n) else x.drop(n))
    case _        => toSEQ.drop(n)

  def has(t: ANY): Boolean = this match
    case SEQ(x) => x.contains(t)
    case ARR(x) => x.contains(t)
    case MAP(x) => x.contains(t)
    case STR(x) => x.contains(t.toString)
    case _      => toSEQ.has(t)

  def shuffle: ANY = this match
    case SEQ(x)   => Random.shuffle(x).toSEQ
    case ARR(x)   => Random.shuffle(x).toARR
    case MAP(x)   => Random.shuffle(x).toMAP
    case STR(x)   => STR(Random.shuffle(x).toString)
    case FN(p, x) => Random.shuffle(x).pFN(p)
    case _        => toSEQ.shuffle

  def join(s: String): String = this match
    case SEQ(x)   => x.mkString(s)
    case ARR(x)   => x.mkString(s)
    case _: STR   => toSEQ.join(s)
    case FN(_, x) => x.mkString(s)
    case _        => toString

  def permutations: ANY = this match
    case SEQ(x)   => x.permutations.map(_.toSEQ).toSEQ
    case ARR(x)   => x.permutations.map(_.toARR).toSEQ
    case _: MAP   => toARR.permutations.map(_.toMAP)
    case FN(p, x) => x.permutations.map(_.pFN(p)).toSEQ
    case STR(x)   => x.permutations.map(STR.apply).toSEQ
    case _        => toSEQ.permutations

  def combinations(n: Int): ANY = this match
    case SEQ(x)   => x.combinations(n).map(_.toSEQ).toSEQ
    case ARR(x)   => x.combinations(n).map(_.toARR).toSEQ
    case _: MAP   => toARR.combinations(n).map(_.toMAP)
    case FN(p, x) => x.combinations(n).map(_.pFN(p)).toSEQ
    case STR(x)   => x.combinations(n).map(STR.apply).toSEQ
    case _        => toSEQ.combinations(n)

  def toSEQ: SEQ = this match
    case x: SEQ => x
    case ARR(x) => LazyList.from(x).toSEQ
    case MAP(x) =>
      LazyList.from(x).map { case (i, a) => Vector(i, a).toARR }.toSEQ
    case STR(x)   => LazyList.from(x).map(c => STR(c.toString)).toSEQ
    case FN(_, x) => LazyList.from(x).toSEQ
    case UN       => LazyList.empty.toSEQ
    case _        => LazyList(this).toSEQ

  def toARR: ARR = this match
    case x: ARR => x
    case SEQ(x) => x.toARR
    case _      => toSEQ.toARR

  def toMAP: MAP = this match
    case x: MAP => x
    case SEQ(x) =>
      x.flatMap {
        case Itr(a) if a.length > 0 => Some((a.get(NUM(0)), a.get(NUM(1))))
        case _                      => None
      }.to(VectorMap).toMAP
    case _ => toSEQ.toMAP

  def toSTR: STR = STR(toString)

  def toNUM: NUM = this match
    case x: NUM => x
    case STR(x) =>
      try x.toNUM
      catch
        case e: java.lang.NumberFormatException =>
          throw LinEx("NUM", s"""bad cast "$x"""")
    case UN => NUM(0)
    case _  => toSTR.toNUM

  def optNUM: Option[NUM] =
    try Some(toNUM)
    catch _ => None

  def toI: Int = toNUM.x.intValue

  def optI: Option[Int] = optNUM.map(_.x.intValue)

  /** Converts `ANY` to `FN` body. */
  def xFN: List[ANY] = this match
    case FN(_, x) => x
    case STR(x)   => Parser.parse(x)
    case SEQ(x)   => x.toList
    case _        => toSEQ.xFN

  def toFN(env: ENV): FN = FN(env.code.p, xFN)

  /** Converts `ANY` to `FN` at given line number.
    *
    * @param l
    *   line number
    * @param env
    *   context `ENV` to wrap `FN`
    */
  def lFN(l: Int, env: ENV): FN = FN(PATH(env.code.p.f, l), xFN)

  /** Converts `ANY` to `FN` at given `PATH`.
    *
    * @param p
    *   `PATH`
    * @param env
    *   context `ENV` to wrap `FN`
    */
  def pFN(p: PATH): FN = FN(p, xFN)

  def matchType(a: ANY): ANY = a match
    case _: SEQ               => toSEQ
    case _: ARR               => toARR
    case _: MAP               => toMAP
    case _: STR               => toSTR
    case _: NUM               => toNUM
    case _: CMD               => toString.pipe(CMD.apply)
    case FN(p, _)             => pFN(p)
    case ERR(LinERR(p, t, _)) => LinERR(p, t, toString).pipe(ERR.apply)
    case UN                   => UN

  def map(f: ANY => ANY): ANY = this match
    case SEQ(x)   => x.map(f).toSEQ
    case ARR(x)   => x.map(f).toARR
    case FN(p, x) => x.map(f).pFN(p)
    case _        => toSEQ.map(f)
  def mapM(f: (ANY, ANY) => (ANY, ANY), g: ANY => ANY): ANY = this match
    case MAP(x) => x.map { case (a, b) => f(a, b) }.toMAP
    case _      => map(g)

  def flatMap(f: ANY => ANY): ANY = this match
    case SEQ(x)   => x.flatMap(f(_).toSEQ.x).toSEQ
    case ARR(x)   => x.flatMap(f(_).toARR.x).toARR
    case FN(p, x) => x.flatMap(f(_).toARR.x).pFN(p)
    case _        => toSEQ.flatMap(f)
  def flatMapM(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) => x.flatMap { case (a, b) => f(a, b).toARR.x }.toARR
    case _      => flatMap(g)
  def flat: ANY = flatMap(x => x)

  def zip(t: ANY, f: (ANY, ANY) => ANY): ANY = (this, t) match
    case (MAP(x), Itr(_)) =>
      x.foldLeft(VectorMap[ANY, ANY]())((a, b) =>
        val (k, v) = b
        t.get(k) match
          case UN => a
          case w  => a + (k -> f(v, w))
      ).toMAP
    case (Itr(_), _: MAP) => t.zip(this, (x, y) => f(y, x))
    case (_: MAP, _)      => zip(t.toSEQ, f)
    case (_, _: MAP)      => toSEQ.zip(t, f)
    case (ARR(x), _)      => x.zip(t.toSEQ.x).map { case (x, y) => f(x, y) }.toARR
    case (_, _: ARR)      => t.zip(this, (x, y) => f(y, x)).toARR
    case _                => toSEQ.x.zip(t.toSEQ.x).map { case (x, y) => f(x, y) }.toSEQ

  def table(t: ANY, f: (ANY, ANY) => ANY): ANY = map(x => t.map(y => f(x, y)))

  def foldLeft[T](a: T)(f: (T, ANY) => T): T = this match
    case SEQ(x) => x.foldLeft(a)(f)
    case ARR(x) => x.foldLeft(a)(f)
    case _      => toSEQ.foldLeft(a)(f)
  def foldLeftM[T](a: T)(f: (T, (ANY, ANY)) => T, g: (T, ANY) => T): T =
    this match
      case MAP(x) => x.foldLeft(a)((b, c) => f(b, c))
      case _      => foldLeft(a)(g)

  def filter(f: ANY => Boolean): ANY = this match
    case SEQ(x)   => x.filter(f).toSEQ
    case ARR(x)   => x.filter(f).toARR
    case FN(p, x) => x.filter(f).pFN(p)
    case _        => toSEQ.filter(f)
  def filterM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.filter { case (a, b) => f(a, b) }.toMAP
    case _      => filter(g)

  def any(f: ANY => Boolean): Boolean = this match
    case SEQ(x)   => x.exists(f)
    case ARR(x)   => x.exists(f)
    case FN(p, x) => x.exists(f)
    case _        => toSEQ.any(f)
  def anyM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): Boolean = this match
    case MAP(x) => x.exists { case (a, b) => f(a, b) }
    case _      => any(g)

  def all(f: ANY => Boolean): Boolean = this match
    case SEQ(x)   => x.forall(f)
    case ARR(x)   => x.forall(f)
    case FN(p, x) => x.forall(f)
    case _        => toSEQ.all(f)
  def allM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): Boolean = this match
    case MAP(x) => x.forall { case (a, b) => f(a, b) }
    case _      => all(g)

  def takeWhile(f: ANY => Boolean): ANY = this match
    case SEQ(x)   => x.takeWhile(f).toSEQ
    case ARR(x)   => x.takeWhile(f).toARR
    case FN(p, x) => x.takeWhile(f).pFN(p)
    case _        => toSEQ.takeWhile(f)
  def takeWhileM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.takeWhile { case (a, b) => f(a, b) }.toMAP
    case _      => takeWhile(g)

  def dropWhile(f: ANY => Boolean): ANY = this match
    case SEQ(x)   => x.dropWhile(f).toSEQ
    case ARR(x)   => x.dropWhile(f).toARR
    case FN(p, x) => x.dropWhile(f).pFN(p)
    case _        => toSEQ.dropWhile(f)
  def dropWhileM(f: (ANY, ANY) => Boolean, g: ANY => Boolean): ANY = this match
    case MAP(x) => x.dropWhile { case (a, b) => f(a, b) }.toMAP
    case _      => dropWhile(g)

  def find(f: ANY => Boolean): Option[ANY] = this match
    case SEQ(x)   => x.find(f)
    case ARR(x)   => x.find(f)
    case FN(p, x) => x.find(f)
    case _        => toSEQ.find(f)
  def findM(
      f: (ANY, ANY) => Boolean,
      g: ANY => Boolean
  ): Option[ANY | (ANY, ANY)] = this match
    case MAP(x) => x.find { case (a, b) => f(a, b) }
    case _      => find(g)

  def uniqBy(f: ANY => ANY): ANY = this match
    case SEQ(x)   => x.distinctBy(f).toSEQ
    case ARR(x)   => x.distinctBy(f).toARR
    case FN(p, x) => x.distinctBy(f).pFN(p)
    case _        => toSEQ.uniqBy(f)
  def uniqByM(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) =>
      x.toSeq.distinctBy { case (a, b) => f(a, b) }.to(VectorMap).toMAP
    case _ => uniqBy(g)

  def sortBy(f: ANY => ANY): ANY = this match
    case SEQ(x)   => x.sortBy(f)(OrdANY).toSEQ
    case ARR(x)   => x.sortBy(f)(OrdANY).toARR
    case FN(p, x) => x.sortBy(f)(OrdANY).pFN(p)
    case _        => toSEQ.sortBy(f)
  def sortByM(f: (ANY, ANY) => ANY, g: ANY => ANY): ANY = this match
    case MAP(x) =>
      x.toSeq.sortBy { case (a, b) => f(a, b) }(OrdANY).to(VectorMap).toMAP
    case _ => sortBy(g)

  def sortWith(f: (ANY, ANY) => Boolean): ANY = this match
    case SEQ(x)   => x.sortWith(f).toSEQ
    case ARR(x)   => x.sortWith(f).toARR
    case FN(p, x) => x.sortWith(f).pFN(p)
    case _        => toSEQ.sortWith(f)
  def sortWithM(
      f: (ANY, ANY, ANY, ANY) => Boolean,
      g: (ANY, ANY) => Boolean
  ): ANY = this match
    case MAP(x) =>
      x.toSeq.sortWith { case ((i, a), (j, b)) => f(i, j, a, b) }
        .to(VectorMap)
        .toMAP
    case _ => sortWith(g)

  /** Vectorizes function over `ANY`.
    *
    * @param f
    *   function to vectorize
    */
  def vec1(f: ANY => ANY): ANY = this match
    case Itr(_) => map(_.vec1(f))
    case _      => f(this)

  /** Vectorizes function over 2 `ANY`s.
    *
    * @param f
    *   function to vectrorize
    */
  def vec2(t: ANY, f: (ANY, ANY) => ANY): ANY = (this, t) match
    case (Itr(_), Itr(_)) => zip(t, _.vec2(_, f))
    case (Itr(_), _)      => map(f(_, t))
    case (_, Itr(_))      => t.map(f(this, _))
    case _                => f(this, t)

  def vef1[T](a: T)(f: (T, ANY) => T): T = this match
    case Itr(_) => foldLeft(a)((x, y) => y.vef1(x)(f))
    case _      => f(a, this)

  def num1(f: NUMF => NUMF): ANY = vec1(x => NUM(f(x.toNUM.x)))
  def num1(f: NUMF => NUMF, e: String): ANY =
    try num1(f)
    catch case _: ArithmeticException => throw LinEx("MATH", e)

  def num2(t: ANY, f: (NUMF, NUMF) => NUMF): ANY =
    vec2(t, (x, y) => NUM(f(x.toNUM.x, y.toNUM.x)))
  def num2(t: ANY, f: (NUMF, NUMF) => NUMF, e: String): ANY =
    try num2(t, f)
    catch case _: ArithmeticException => throw LinEx("MATH", e)

  def num2q(t: ANY, f: (NUMF, NUMF) => Iterator[NUMF]): ANY =
    vec2(t, (x, y) => f(x.toNUM.x, y.toNUM.x).map(NUM(_)).toSEQ)

  def num2a(t: ANY, f: (NUMF, NUMF) => Iterable[NUMF]): ANY =
    vec2(t, (x, y) => f(x.toNUM.x, y.toNUM.x).map(NUM(_)).toARR)

  def str1(f: String => String): ANY = vec1(_.toString.pipe(f).pipe(STR.apply))

  def str1a(f: String => Iterable[String]): ANY = vec1(
    _.toString.pipe(f).map(STR(_)).toARR
  )

  def str2(t: ANY, f: (String, String) => String): ANY =
    vec2(t, (x, y) => STR(f(x.toString, y.toString)))

  def str2q(t: ANY, f: (String, String) => Iterator[String]): ANY =
    vec2(t, (x, y) => f(x.toString, y.toString).map(STR(_)).toSEQ)

  def str2a(t: ANY, f: (String, String) => Iterable[String]): ANY =
    vec2(t, (x, y) => f(x.toString, y.toString).map(STR(_)).toARR)

  def strnum(t: ANY, f: (String, NUMF) => String): ANY =
    vec2(t, (x, y) => STR(f(x.toString, y.toNUM.x)))

  def strnuma(t: ANY, f: (String, NUMF) => Iterator[String]): ANY =
    vec2(t, (x, y) => f(x.toString, y.toNUM.x).map(STR(_)).toSEQ)

object OrdANY extends Ordering[ANY]:

  def compare(x: ANY, y: ANY): Int = x.cmp(y)

object ANY:

  extension (x: Iterable[ANY])

    def toSEQ: SEQ       = SEQ(x.to(LazyList))
    def toARR: ARR       = ARR(x.toVector)
    def pFN(p: PATH): FN = FN(p, x.toList)

  extension (x: Iterator[ANY])

    def toSEQ: SEQ       = SEQ(x.to(LazyList))
    def pFN(p: PATH): FN = FN(p, x.toList)

  extension (x: MAPW[ANY, ANY]) def toMAP: MAP = MAP(x)

  extension (b: Boolean)

    def boolI: Int   = if b then 1 else 0
    def boolNUM: NUM = NUM(b.boolI)

  extension (s: String) def toNUM: NUM = NUM(Real(s))

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
      case _: SEQ | _: ARR | _: STR | _: FN => Some(a)
      case _                                => None

  def fromDec(n: SafeLong, b: Int): Vector[SafeLong] =
    def loop(n: SafeLong): Vector[SafeLong] =
      if b == 1 then Vector.fill(n.toInt)(1)
      else if n == 0 then Vector.empty
      else loop(n / b) :+ n % b
    if b < 1 then throw LinEx("MATH", s"bad base $b")
    loop(n) match
      case Vector() if b > 1 => Vector(0)
      case x                 => x

  def toDec(ns: Vector[SafeLong], b: SafeLong): SafeLong =
    if b < 1 then throw LinEx("MATH", s"bad base $b")
    if b == 1 then ns.length
    else
      ns.reverseIterator.zipWithIndex.foldLeft[SafeLong](0) {
        case (a, (n, i)) =>
          b ** i * n + a
      }

  def baseN[A](seed: Vector[A], n: Int) =
    if n < 0 then throw LinEx("MATH", s"bad length $n")
    val l = seed.length
    LazyList.unfold(SafeLong(0))(i =>
      if i < SafeLong(l) ** n then
        Some(
          seed
            .map(_ => SafeLong(0))
            .++(ANY.fromDec(i, seed.length))
            .takeRight(n)
            .map(_.toInt.pipe(seed)),
          i + 1
        )
      else None
    )
