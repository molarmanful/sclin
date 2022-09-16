import scala.util.chaining._
import spire.math._

object Parser:

  object PT:

    val UN  = 0
    val STR = 1
    val NUM = 2
    val CMD = 3
    val ESC = 4
    val DEC = 5

  case class PST(xs: List[ANY], x: String, t: Int)

  def isPar(s: String) = s.forall("()[]{}".contains(_))

  def clean(st: PST) = st.t match
    case PT.UN => st
    case _ =>
      val xs = st.t match
        case PT.CMD if isPar(st.x) =>
          st.xs ++ st.x.map(c => ANY.CMD(c.toString)).toList
        case _ =>
          st.xs :+ (
            if st.x == "." then ANY.CMD(".")
            else
              st.t match
                case PT.STR          => ANY.STR(st.x)
                case PT.CMD          => ANY.CMD(st.x)
                case PT.DEC | PT.NUM => ANY.NUM(Rational(st.x))
          )
      PST(xs, "", PT.UN)

  def addc(c: String | Char, st: PST) = st.copy(x = st.x + c)
  def addt(t: Int, st: PST)           = st.copy(t = t)
  def newc(c: String | Char, st: PST) = clean(st).copy(x = c.toString)

  def pstr(st: PST, c: Char) = st.t match
    case PT.ESC =>
      val s = c match
        case '"' => "\""
        case _   => "\\" + c
      addc(s, st).pipe(addt(PT.STR, _))
    case PT.STR =>
      c match
        case '\\' => addt(PT.ESC, st)
        case '"'  => clean(st)
        case _    => addc(c, st)

  def pnum(st: PST, c: Char) = st.t match
    case PT.DEC | PT.NUM => addc(c, st)
    case _               => newc(c, st).pipe(addt(PT.NUM, _))

  def pdot(st: PST) = (st.t match
    case PT.NUM => addc
    case _      => newc
  )('.', st).pipe(addt(PT.DEC, _))

  def pcmd(st: PST, c: Char) = st.t match
    case PT.CMD => addc(c, st)
    case _      => newc(c, st).pipe(addt(PT.CMD, _))

  def choice(st: PST, c: Char) = st.t match
    case PT.STR | PT.ESC => st
    case _ =>
      if c.isDigit then pnum(st, c)
      else
        c match
          case '"'                      => clean(st).pipe(addt(PT.STR, _))
          case '.'                      => pdot(st)
          case ' ' | '\t' | '\r' | '\n' => clean(st)
          case _                        => pcmd(st, c)

  def pline(s: String) =
    s.foldLeft(PST(List[ANY](), "", PT.UN))(choice).pipe(clean).xs

  def parse(s: String) = s.split("\n").headOption.getOrElse("").pipe(pline)
