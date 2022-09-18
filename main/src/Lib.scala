import scala.util.chaining._

implicit class LIB(env: ENV):

  def startFN: ENV =
    def loop(
        code: List[ANY],
        d: Int = 1,
        res: List[ANY] = List()
    ): (List[ANY], List[ANY]) =
      if d > 0 then
        code match
          case List() => (code, res)
          case c :: cs =>
            loop(
              cs,
              c match
                case ANY.CMD(x) if x.contains('(') => d + 1
                case ANY.CMD(x) if x.contains(')') => d - 1
                case _                             => d
              ,
              res :+ c
            )
      else
        (
          code,
          res.lastOption
            .getOrElse(ANY.CMD(")"))
            .toString
            .split("\\)")
            .lastOption match
            case Some(c) =>
              (ANY.CMD("(") :: res.dropRight(1)) :+ ANY.CMD(")") :+ ANY.CMD(c)
            case _ => res
        )
    val (code, res) = loop(env.code.x)
    env.modCode(_ => code).push(ANY.FN(env.code.p, res))

  def startARR = env.setArr(env.stack :: env.arr).clr

  def endARR = env.arr match
    case List()  => env
    case x :: xs => env.setArr(xs).modStack(_ => x).push(ANY.ARR(env.stack))

  def endMAP = endARR.toMAP

  def toSEQ = env.mod1(_.toSEQ)
  def toARR = env.mod1(_.toARR)
  def toMAP = env.mod1(_.toMAP)
  def toSTR = env.mod1(_.toSTR)
  def toNUM = env.mod1(_.toNUM)
  def toFN  = env.mod1(_.toFN(env))

  def out: ENV = env.arg1((x, env) =>
    print(x); env
  )

  def outn: ENV = env.arg1((x, env) =>
    println(x); env
  )

  def form: ENV = env.mod1(x => ANY.STR(x.toForm))
  def outf: ENV = env.form.outn

  def dup: ENV  = env.mods1(x => Vector(x, x))
  def dups: ENV = env.push(ANY.ARR(env.stack))
  def over: ENV = env.mods2((x, y) => Vector(x, y, x))

  def pop: ENV = env.mods1(_ => Vector())
  def clr: ENV = env.modStack(_ => Vector())
  def nip: ENV = env.mod2((x, _) => x)

  def swap: ENV = env.mods2((x, y) => Vector(y, x))
  def rev: ENV  = env.modStack(_.reverse)
  def tuck: ENV = env.mods2((x, y) => Vector(y, x, y))

  def rot: ENV  = env.mods3((x, y, z) => Vector(y, z, x))
  def rotu: ENV = env.mods3((x, y, z) => Vector(z, x, y))

  def dot: ENV = ???

  def cmd(x: String): ENV = x match

    // TYPES
    case "("    => startFN
    case ")"    => env // TODO: ?
    case "["    => startARR
    case "]"    => endARR
    case ">Q"   => toSEQ
    case ">A"   => toARR
    case ">M"   => toMAP
    case ">S"   => toSTR
    case ">N"   => toNUM
    case ">F"   => toFN
    case ">E"   => ???
    case "form" => form

    // I/O
    case ">O"  => out
    case "n>O" => outn
    case "f>O" => outf

    // STACK
    case "dup"   => dup
    case "dups"  => dups
    case "over"  => over
    case "pick"  => ???
    case "pop"   => pop
    case "clr"   => clr
    case "nip"   => nip
    case "nix"   => ???
    case "swap"  => swap
    case "rev"   => rev
    case "tuck"  => tuck
    case "trade" => ???
    case "rot"   => rot
    case "roll"  => rotu
    case "roll_" => ???
    case "dip"   => ???

    // MATH
    case "+"  => ???
    case "-"  => ???
    case "*"  => ???
    case "/"  => ???
    case "%"  => ???
    case "/%" => ???
    case "^"  => ???

    // CONSTANTS
    case "UN" => env.push(ANY.UN)
    case "()" => env.push(ANY.UN)
    case "[]" => env.push(ANY.UN)

    // MAGIC DOT
    case "." => dot

    case _ => throw LinERR(env.code.p, "FN", s"unknown fn \"$x\"")
