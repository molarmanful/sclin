import java.util.Formatter
import org.apfloat._
import util.chaining._
import NUMF._

/** ADT for lin types. */
enum ANY:

  case SEQ(x: SEQW)
  case ARR(x: ARRW)
  case MAP(x: MAPW)
  case STR(x: String)
  case NUM(x: NUMF)
  case CMD(x: String)
  case FN(p: PATH, x: List[ANY])
  case ERR(x: LinERR)
  case UN

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

  /** Gets type name of `ANY`. */
  def getType: String = getClass.getSimpleName

  /** `toString` override for `ANY`. */
  override def toString: String = this match
    case SEQ(x) => x.mkString
    case ARR(x) => x.mkString(" ")
    case MAP(x) =>
      x.iterator.map { case (i, a) => i.toString + " " + a.toString }
        .mkString("\n")
    case STR(x)   => x
    case NUM(x)   => x.toString(true)
    case CMD(x)   => x
    case FN(_, x) => x.mkString(" ")
    case ERR(x)   => x.toString
    case UN       => ""

  /** Converts `ANY` to formatted string. */
  def toForm: String = this match
    case _: SEQ => s"[…]"
    case ARR(x) => s"[${x.map(_.toForm).mkString(" ")}]"
    case MAP(x) =>
      s"{${x.iterator.map { case (i, a) => i.toForm + "=>" + a.toForm }
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

  /** Converts `ANY` to boolean. */
  def toBool: Boolean = this match
    case SEQ(x)   => !x.isEmpty
    case ARR(x)   => !x.isEmpty
    case MAP(x)   => !x.isEmpty
    case STR(x)   => !x.isEmpty
    case NUM(x)   => x != 0
    case CMD(x)   => !x.isEmpty
    case FN(_, x) => !x.isEmpty
    case _: ERR   => false
    case UN       => false

  /** Gets length of `ANY`. */
  def length: Int = this match
    case SEQ(x)   => x.length
    case ARR(x)   => x.length
    case MAP(x)   => x.size
    case STR(x)   => x.length
    case FN(_, x) => x.length
    case _        => 0

  /** Gets `ANY` from `ANY`.
    *
    * @param i
    *   index to retrieve
    */
  def get(i: ANY): ANY =
    this match
      case Its(x) if i.optI != None =>
        val i1 = i.toI
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

  /** Converts `ANY` to `SEQ`. */
  def toSEQ: SEQ = this match
    case SEQ(x) => x.toSEQ
    case ARR(x) => LazyList.from(x).toSEQ
    case MAP(x) =>
      LazyList.from(x).map { case (i, a) => Vector(i, a).toARR }.toSEQ
    case STR(x)   => LazyList.from(x).map(c => STR(c.toString)).toSEQ
    case FN(_, x) => LazyList.from(x).toSEQ
    case UN       => LazyList[ANY]().toSEQ
    case _        => LazyList(this).toSEQ

  /** Converts `ANY` to `ARR`. */
  def toARR: ARR = this match
    case ARR(x) => x.toARR
    case SEQ(x) => x.toVector.toARR
    case _      => toSEQ.toARR

  /** Converts `ANY` to `MAP`. */
  def toMAP: MAP = this match
    case MAP(x) => x.toMAP
    case SEQ(x) =>
      x.flatMap {
        case Itr(a) if a.length > 0 => Some((a.get(NUM(0)), a.get(NUM(1))))
        case _                      => None
      }.toMap.toMAP
    case _ => toSEQ.toMAP

  /** Converts `ANY` to `STR`. */
  def toSTR: STR = STR(toString)

  /** Converts `ANY` to `NUM`. */
  def toNUM: NUM = this match
    case NUM(x) => NUM(x)
    case STR(x) => NUM(x)
    case UN     => NUM(0)
    case _      => toSTR.toNUM

  /** Converts `ANY` to `NUM` without failing. */
  def optNUM: Option[NUM] =
    try Some(toNUM)
    catch _ => None

  /** Converts `ANY` to int. */
  def toI: Int = toNUM.x.intValue

  /** Converts `ANY` to int without failing. */
  def optI: Option[Int] = optNUM.map(_.x.intValue)

  /** Converts `ANY` to `FN` body. */
  def toFNx: List[ANY] = this match
    case FN(_, x) => x
    case STR(x)   => Parser.parse(x)
    case SEQ(x)   => x.toList
    case _        => toSEQ.toFNx

  /** Converts `ANY` to `FN`.
    *
    * @param env
    *   context `ENV` to wrap `FN`
    */
  def toFN(env: ENV): FN = FN(env.code.p, toFNx)

  /** Converts `ANY` to `FN` at given line number.
    *
    * @param l
    *   line number
    * @param env
    *   context `ENV` to wrap `FN`
    */
  def iFN(l: Int, env: ENV): FN = FN(PATH(env.code.p.f, l), toFNx)

  /** Maps function over `ANY`.
    *
    * @param f
    *   function to map with
    */
  def map(f: ANY => ANY): ANY = this match
    case SEQ(x) => x.map(f).toSEQ
    case ARR(x) => x.map(f).toARR
    case MAP(x) => x.map { case (a, b) => (a, f(b)) }.toMAP
    case _      => toSEQ.map(f)

  /** Zips 2 `ANY`s using function.
    *
    * @param t
    *   second `ANY` to zip with
    * @param f
    *   function to zip with
    * @return
    */
  def zip(t: ANY)(f: (ANY, ANY) => ANY): ANY = (this, t) match
    case (MAP(x), Itr(_)) =>
      x.foldLeft(Map[ANY, ANY]())((a, b) =>
        val (k, v) = b
        t.get(k) match
          case UN => a
          case w  => a + (k -> f(v, w))
      ).toMAP
    case (Itr(_), _: MAP) => t.zip(this)((x, y) => f(y, x))
    case (_: MAP, _)      => zip(t.toSEQ)(f)
    case (_, _: MAP)      => toSEQ.zip(t)(f)
    case (ARR(x), _)      => x.zip(t.toSEQ.x).map { case (x, y) => f(x, y) }.toARR
    case (_, _: ARR)      => t.zip(this)((x, y) => f(y, x))
    case _                => toSEQ.x.zip(t.toSEQ.x).map { case (x, y) => f(x, y) }.toSEQ

  /** Recursively maps over `ANY`.
    *
    * @param f
    *   function to map with
    */
  def vec1(f: ANY => ANY): ANY = this match
    case Itr(_) => map(_.vec1(f))
    case _      => f(this)

  def vec2(t: ANY)(f: (ANY, ANY) => ANY): ANY = (this, t) match
    case (Itr(_), Itr(_)) => zip(t)(_.vec2(_)(f))
    case (Itr(_), _)      => map(f(_, t))
    case (_, Itr(_))      => map(f(t, _))
    case _                => f(this, t)

object ANY:

  extension (x: SEQW) def toSEQ: SEQ = SEQ(x)

  extension (x: ARRW) def toARR: ARR = ARR(x)

  extension (x: MAPW) def toMAP: MAP = MAP(x)

  extension (b: Boolean) def boolNUM: NUM = NUM(if b then 1 else 0)
