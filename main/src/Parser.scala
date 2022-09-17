import scala.util.chaining._
import spire.math._

object PT:

  val UN  = 0
  val STR = 1
  val NUM = 2
  val CMD = 3
  val ESC = 4
  val DEC = 5

case class Parser(xs: List[ANY], x: String, t: Int):

  def clean = t match
    case PT.UN => this
    case _ =>
      val xs1 = t match
        case PT.CMD if Parser.isPar(x) =>
          xs ++ x.map(c => ANY.CMD(c.toString)).toList
        case _ =>
          xs :+ (
            if x == "." then ANY.CMD(".")
            else
              t match
                case PT.STR          => ANY.STR(x)
                case PT.CMD          => ANY.CMD(x)
                case PT.DEC | PT.NUM => ANY.NUM(Rational(x))
          )
      Parser(xs1, "", PT.UN)

  def addc(c: String | Char) = copy(x = x + c)
  def addt(t: Int)           = copy(t = t)
  def newc(c: String | Char) = clean.copy(x = c.toString)

  def pstr(c: Char) = t match
    case PT.ESC =>
      val s = c match
        case '"' => "\""
        case _   => "\\" + c
      addc(s).addt(PT.STR)
    case PT.STR =>
      c match
        case '\\' => addt(PT.ESC)
        case '"'  => clean
        case _    => addc(c)

  def pnum(c: Char) = t match
    case PT.DEC | PT.NUM => addc(c)
    case _               => newc(c).addt(PT.NUM)

  def pdot = (t match
    case PT.NUM => addc
    case _      => newc
  )('.').addt(PT.DEC)

  def pcmd(c: Char) = t match
    case PT.CMD => addc(c)
    case _      => newc(c).addt(PT.CMD)

  def choice(c: Char) = t match
    case PT.STR | PT.ESC => pstr(c)
    case _ =>
      c match
        case '"'                      => clean.addt(PT.STR)
        case '.'                      => pdot
        case c if c.isDigit           => pnum(c)
        case ' ' | '\t' | '\r' | '\n' => clean
        case _                        => pcmd(c)

object Parser:

  def isPar(s: String) = s.forall("()[]{}".contains(_))

  def pline(s: String) =
    s.foldLeft(Parser(List(), "", PT.UN))((st, c) => st.choice(c)).clean.xs

  def parse(s: String) = s.split("\n").headOption.getOrElse("").pipe(pline)
