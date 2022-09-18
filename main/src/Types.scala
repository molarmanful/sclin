import pprint.Tree.Lazy
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

  type Itr = SEQ | ARR | MAP
  type It  = SEQ | ARR

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
      s"(${x.mkString(" ")})$n"
    case ERR(x) => s"ERR(${x.t})"
    case UN     => "UN"
    case _      => toString

  /** Converts `ANY` to boolean. */
  def toBool: Boolean = ???

  /** Converts `ANY` to `SEQ`. */
  def toSEQ: ANY = this match
    case SEQ(_) => this
    case ARR(x) => SEQ(LazyList.from(x))
    case MAP(x) =>
      SEQ(LazyList.from(x).map { case (i, a) => ARR(Vector(i, a)) })
    case STR(x)   => SEQ(LazyList.from(x).map(c => STR(c.toString)))
    case FN(_, x) => SEQ(LazyList.from(x))
    case UN       => SEQ(LazyList())
    case _        => SEQ(LazyList(this))

  /** Converts `ANY` to `ARR`. */
  def toARR: ANY = this match
    case ARR(_) => this
    case SEQ(x) => ARR(x.toVector)
    case _      => toSEQ.toARR

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
