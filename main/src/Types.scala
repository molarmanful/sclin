import spire.algebra._
import spire.implicits._
import spire.math._
import util.chaining._

type FILE = Option[os.Path]
case class PATH(f: FILE, l: Int)

enum ANY:

  case SEQ(x: LazyList[ANY])
  case ARR(x: Vector[ANY])
  case MAP(x: Map[ANY, ANY])
  case STR(x: String)
  case NUM(x: Rational)
  case CMD(x: String)
  case FN(p: PATH, x: List[ANY])
  case UN

  type Itr = SEQ | ARR | MAP
  type It  = SEQ | ARR

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
    case UN       => ""

  def toForm: String = this match
    case SEQ(_) => s"[…]"
    case ARR(x) => s"[${x.map(_.toForm).mkString(" ")}]"
    case MAP(x) =>
      s"{${x.iterator.map { case (i, a) => i.toForm + "=>" + a.toForm }
          .mkString(" ")}}"
    case STR(x) =>
      s"\"${x.map {
          case '"' => "\\\""
          case c   => c
        }.mkString}\""
    case FN(PATH(_, l), x) =>
      val n = l.toString.map(c => "⁰¹²³⁴⁵⁶⁷⁸⁹" (c - '0'))
      s"(${x.mkString(" ")})$n"
    case UN => "UN"
    case _  => toString

  def toSEQ = this match
    case SEQ(_) => this
    case ARR(x) => SEQ(LazyList.from(x))
    case MAP(x) =>
      SEQ(LazyList.from(x).map { case (i, a) => ARR(Vector(i, a)) })
    case STR(x)   => SEQ(LazyList.from(x).map(c => STR(c.toString)))
    case FN(_, x) => SEQ(LazyList.from(x))
    case UN       => SEQ(LazyList())
    case _        => SEQ(LazyList(this))

  def toFNx: List[ANY] = this match
    case FN(_, x) => x
    case STR(x)   => Parser.parse(x)
    case SEQ(x)   => x.toList
    case _        => toSEQ.toFNx

  def toFN(env: ENV)        = FN(env.code.p, toFNx)
  def iFN(l: Int, env: ENV) = FN(PATH(env.code.p.f, l), toFNx)
