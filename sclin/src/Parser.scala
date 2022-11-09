package sclin

import scala.util.chaining._
import ANY._

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
case class Parser(xs: List[ANY] = List.empty, x: String = "", t: PT = PT.UN):

  /** Pushes `t` with type `t` to `xs`; resets `x` and `t`. */
  def clean: Parser = Parser(t match
    case PT.STR => xs :+ STR(x)
    case PT.ESC => xs :+ STR(x + "\\")
    case PT.CMD =>
      if Parser.isPar(x) then xs ++ x.map(_.toString.pipe(CMD.apply)).toList
      else xs :+ CMD(x)
    case PT.DEC =>
      x match
        case "."    => xs :+ CMD(".")
        case s"$x." => xs :+ x.toNUM :+ CMD(".")
        case _      => xs :+ x.toNUM
    case PT.NUM => xs :+ x.toNUM
    case _      => xs
  )

  /** Adds string/char to `x`.
    *
    * @param c
    *   string/char to add
    */
  def addc(c: String | Char): Parser = copy(x = x + c)

  /** Sets `t`.
    *
    * @param t
    *   type to set
    */
  def sett(t: PT): Parser = copy(t = t)

  /** Parses to STR.
    *
    * @param c
    *   char to add
    */
  def pstr(c: Char): Parser = t match
    case PT.ESC =>
      addc(c match
        case '"' => "\""
        case _   => "\\" + c
      ).sett(PT.STR)
    case _ =>
      c match
        case '\\' => sett(PT.ESC)
        case '"'  => clean
        case _    => addc(c)

  /** Parses to NUM.
    *
    * @param c
    *   char to add
    */
  def pnum(c: Char): Parser = t match
    case PT.DEC | PT.NUM => addc(c)
    case _               => clean.addc(c).sett(PT.NUM)

  /** Handles dot special case. */
  def pdot: Parser = (t match
    case PT.NUM => addc
    case _      => clean.addc
  )('.').sett(PT.DEC)

  /** Parses to CMD.
    *
    * @param c
    *   char to add
    */
  def pcmd(c: Char): Parser = t match
    case PT.CMD => addc(c)
    case _      => clean.addc(c).sett(PT.CMD)

  /** Determines parse approach from `c` and `t`.
    *
    * @param c
    *   char to add
    */
  def choice(c: Char): Parser = t match
    case PT.STR | PT.ESC => pstr(c)
    case _ =>
      c match
        case '"'                      => clean.sett(PT.STR)
        case '.'                      => pdot
        case c if c.isDigit           => pnum(c)
        case ' ' | '\t' | '\r' | '\n' => clean
        case _                        => pcmd(c)

/** Frontend for `Parser`. */
object Parser:

  /** Checks if CMD is brackets only.
    *
    * @param s
    *   string to check
    */
  def isPar(s: String): Boolean = s.forall("()[]{}".contains)

  /** Parses a single line.
    *
    * @param s
    *   string to parse
    */
  def pline(s: String): List[ANY] =
    s.foldLeft(Parser())((st, c) => st.choice(c)).clean.xs

  /** Parses an arbitrary string.
    *
    * @param s
    *   string to parse
    */
  def parse(s: String): List[ANY] =
    s.split("\n").headOption.getOrElse("").pipe(pline)
