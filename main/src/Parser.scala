import scala.util.chaining._
import spire.math._

/** Placeholders for parser primitives. */
object PT:

  val UN: Int  = 0
  val STR: Int = 1
  val NUM: Int = 2
  val CMD: Int = 3
  val ESC: Int = 4
  val DEC: Int = 5

/** Parser state.
  *
  * @param xs
  *   parsed atoms
  * @param x
  *   currently parsing atom
  * @param t
  *   type of `x`
  */
case class Parser(xs: List[ANY], x: String, t: Int):

  /** Pushes `t` with type `t` to `xs`; resets `x` and `t`. */
  def clean: Parser = t match
    case PT.UN => this
    case _ =>
      Parser(
        t match
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
        ,
        "",
        PT.UN
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
  def sett(t: Int): Parser = copy(t = t)

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
    case PT.STR =>
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
  def isPar(s: String): Boolean = s.forall("()[]{}".contains(_))

  /** Parses a single line.
    *
    * @param s
    *   string to parse
    */
  def pline(s: String): List[ANY] =
    s.foldLeft(Parser(List(), "", PT.UN))((st, c) => st.choice(c)).clean.xs

  /** Parses an arbitrary string.
    *
    * @param s
    *   string to parse
    */
  def parse(s: String): List[ANY] =
    s.split("\n").headOption.getOrElse("").pipe(pline)
