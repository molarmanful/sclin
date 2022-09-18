import cats._
import spire.algebra._
import spire.implicits._
import spire.math._
import util.chaining._

/** ADT for lin types. */
enum ANY:

  case SEQ(x: LazyList[ANY])
  case ARR(x: Vector[ANY])
  case MAP(x: Map[ANY, ANY])
  case STR(x: String)
  case NUM(x: Rational)
  case CMD(x: String)
  case FN(p: PATH, x: List[ANY])
  case ERR(x: LinERR)
  case UN

  /** Pattern for `SEQ`-like. */
  object Itr:

    def unapply(a: ANY): Option[ANY] = a match
      case SEQ(_) | ARR(_) | MAP(_) => Some(a)
      case _                        => None

  /** Pattern for strict `SEQ`-like. */
  object It:

    def unapply(a: ANY): Option[Any] = a match
      case SEQ(_) | ARR(_) => Some(a)
      case _               => None

  /** Pattern for loose `SEQ`-like. */
  object Its:

    def unapply(a: ANY): Option[ANY] = a match
      case SEQ(_) | ARR(_) | STR(_) | FN(_, _) => Some(a)
      case _                                   => None

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
    case NUM(x)   => x.toString
    case CMD(x)   => x
    case FN(_, x) => x.mkString(" ")
    case ERR(x)   => x.toString
    case UN       => ""

  /** Converts `ANY` to formatted string. */
  def toForm: String = this match
    case SEQ(_) => s"[…]"
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
    case ERR(_)   => false
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
          case CMD(_) => toSTR.get(i)
          case _      => UN

  /** Converts `ANY` to `SEQ`. */
  def toSEQ: SEQ = this match
    case SEQ(x) => SEQ(x)
    case ARR(x) => SEQ(LazyList.from(x))
    case MAP(x) =>
      SEQ(LazyList.from(x).map { case (i, a) => ARR(Vector(i, a)) })
    case STR(x)   => SEQ(LazyList.from(x).map(c => STR(c.toString)))
    case FN(_, x) => SEQ(LazyList.from(x))
    case UN       => SEQ(LazyList())
    case _        => SEQ(LazyList(this))

  /** Converts `ANY` to `ARR`. */
  def toARR: ARR = this match
    case ARR(x) => ARR(x)
    case SEQ(x) => ARR(x.toVector)
    case _      => toSEQ.toARR

  /** Converts `ANY` to `MAP`. */
  def toMAP: MAP = this match
    case MAP(x) => MAP(x)
    case SEQ(x) =>
      MAP(x.flatMap {
        case Itr(a) if a.length > 0 => Some((a.get(NUM(0)), a.get(NUM(1))))
        case _                      => None
      }.toMap)
    case _ => toSEQ.toMAP

  /** Converts `ANY` to `STR`. */
  def toSTR: STR = STR(toString)

  /** Converts `ANY` to `NUM`. */
  def toNUM: NUM = this match
    case NUM(x) => NUM(x)
    case STR(x) => NUM(Rational(x))
    case UN     => NUM(0)
    case _      => toSTR.toNUM

  /** Converts `ANY` to `NUM` without failing. */
  def optNUM: Option[NUM] =
    try Some(toNUM)
    catch _ => None

  /** Converts `ANY` to int. */
  def toI: Int = toNUM.x.toInt

  /** Converts `ANY` to int without failing. */
  def optI: Option[Int] = optNUM.map(_.x.toInt)

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
