package sclin

import scala.util.chaining.*
import ANY.*

/** Parser primitive tags. */
enum PT:

  case UN, STR, NUM, CMD, ESC, DEC

/** Parser state.
  *
  * @param xs
  *   parsed atoms
  * @param x
  *   currently parsing atom
  * @param t
  *   type of `x`
  */
case class Parser(
    xs: LazyList[ANY] = LazyList(),
    x: String = "",
    t: PT = PT.UN
):

  def choice(c: Char): Parser = t match
    case PT.ESC => pesc(c)
    case PT.STR => pstr(c)
    case _ =>
      c match
        case '"'                 => clean.sett(PT.STR)
        case '.'                 => pdot
        case c if c.isDigit      => pnum(c)
        case c if c.isWhitespace => clean
        case _                   => pcmd(c)

  def clean: Parser = Parser:
    t match
      case PT.STR => xs :+ STR(x)
      case PT.ESC => xs :+ STR(x + "\\")
      case PT.CMD =>
        if x.forall("()[]{}".contains) then
          xs ++ x.map(_.toString.pipe(CMD.apply)).toList
        else xs :+ CMD(x)
      case PT.DEC =>
        x match
          case "."    => xs :+ CMD(".")
          case s"$x." => xs :+ x.toNUM :+ CMD(".")
          case _      => xs :+ x.toNUM
      case PT.NUM => xs :+ x.toNUM
      case _      => xs

  def addc(c: String | Char): Parser = copy(x = x + c)
  def sett(t: PT): Parser            = copy(t = t)

  def pesc(c: Char): Parser = addc:
    c match
      case '"' => "\""
      case _   => "\\" + c
  .sett(PT.STR)

  def pstr(c: Char): Parser = c match
    case '\\' => sett(PT.ESC)
    case '"'  => clean
    case _    => addc(c)

  def pnum(c: Char): Parser = t match
    case PT.DEC | PT.NUM => addc(c)
    case _               => clean.addc(c).sett(PT.NUM)

  def pdot: Parser = (t match
    case PT.NUM => addc
    case _      => clean.addc
  )('.').sett(PT.DEC)

  def pcmd(c: Char): Parser = t match
    case PT.CMD => addc(c)
    case _      => clean.addc(c).sett(PT.CMD)

/** Frontend for `Parser`. */
object Parser:

  def pline(s: String): LazyList[ANY] =
    s.foldLeft(Parser())(_.choice(_)).clean.xs

  def parse(s: String): LazyList[ANY] =
    s.split("\n").headOption.getOrElse("").pipe(pline)
