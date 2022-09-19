import scala.util.chaining._
import ANY._

implicit class LIB(env: ENV):

  def eval: ENV =
    env.arg1((x, env) =>
      x match
        case f: FN =>
          val env1 = env.copy(code = f)
          env.code.x match
            case List() => env1.exec
            case _      => env.modStack(_ => env1.exec.stack)
        case f: CMD => env.execA(f)
        case _      => env.push(x).toFN.eval
    )

  def evalA(x: STACK, f: ANY): ANY = env.modStack(_ => x :+ f).eval.getStack(0)

  def quar: ENV = env.push(env.eval.getStack(0))

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
                case CMD(x) if x.contains('(') => d + 1
                case CMD(x) if x.contains(')') => d - 1
                case _                         => d
              ,
              res :+ c
            )
      else
        val res1 = res.dropRight(1)
        res.last.toString match
          case s"$c)$d" =>
            (
              CMD(s")$d") :: code,
              c match
                case "" => res1
                case _  => List(FN(env.code.p, res1)) :+ CMD(c)
            )
    val (code, res) = loop(env.code.x)
    env.modCode(_ => code).push(FN(env.code.p, res))

  def evalLine: ENV = ???

  def startARR: ENV = env.setArr(env.stack :: env.arr).clr

  def endARR: ENV = env.arr match
    case List()  => env
    case x :: xs => env.setArr(xs).modStack(_ => x).push(ARR(env.stack))

  def endMAP: ENV = endARR.toMAP

  def toSEQ: ENV = env.mod1(_.toSEQ)
  def toARR: ENV = env.mod1(_.toARR)
  def toMAP: ENV = env.mod1(_.toMAP)
  def toSTR: ENV = env.mod1(_.toSTR)
  def toNUM: ENV = env.mod1(_.toNUM)
  def toFN: ENV  = env.mod1(_.toFN(env))

  def toERR: ENV =
    env.mod2((x, y) => ERR(LinERR(env.code.p, y.toString, x.toString)))

  def toBool: ENV = env.mod1(_.toBool.toNUM)

  def out: ENV  = env.arg1((x, env) => env.tap(_ => print(x)))
  def outn: ENV = env.arg1((x, env) => env.tap(_ => println(x)))

  def form: ENV = env.mod1(x => STR(x.toForm))
  def outf: ENV = env.form.outn

  def dup: ENV  = env.mods1(x => Vector(x, x))
  def dups: ENV = env.push(ARR(env.stack))
  def over: ENV = env.mods2((x, y) => Vector(x, y, x))

  def pop: ENV = env.mods1(_ => Vector())
  def clr: ENV = env.modStack(_ => Vector())
  def nip: ENV = env.mod2((x, _) => x)

  def swap: ENV = env.mods2((x, y) => Vector(y, x))
  def rev: ENV  = env.modStack(_.reverse)
  def tuck: ENV = env.mods2((x, y) => Vector(y, x, y))

  def rot: ENV  = env.mods3((x, y, z) => Vector(y, z, x))
  def rotu: ENV = env.mods3((x, y, z) => Vector(z, x, y))

  def dip: ENV = env.arg1((x, env) => env.eval.push(x))

  def get: ENV = env.mod2((x, y) => x.get(y))
  def len: ENV = env.mod1(x => NUM(x.length))

  def wrap$ : ENV   = env.mod(2, ARR.apply)
  def wrap: ENV     = env.mod(1, ARR.apply)
  def unwrap: ENV   = env.mods1(_.toARR.x)
  def unwrap$ : ENV = env.arg1((x, env) => env.modStack(_ => x.toARR.x))
  def wrapFN: ENV   = env.wrap.mod1(_.toFN(env))
  def wrapFN$ : ENV = env.wrap$.mod1(_.toFN(env))

  def map: ENV =
    env.mod2((x, y) => y.vec1(f => x.map(a => env.evalA(Vector(a), f))))

  def dot: ENV = ???

  def cmd(x: String): ENV = x match

    // TYPES
    case "("    => startFN
    case ")"    => env // TODO: ?
    case "["    => startARR
    case "]"    => endARR
    case "{"    => startARR
    case "}"    => endMAP
    case ">Q"   => toSEQ
    case ">A"   => toARR
    case ">M"   => toMAP
    case ">S"   => toSTR
    case ">N"   => toNUM
    case ">F"   => toFN
    case ">E"   => toERR
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
    case "dip"   => dip

    // FN/EXEC
    case "#"   => eval
    case "Q"   => quar
    case "\\"  => wrapFN
    case ",\\" => wrapFN$

    // NUM/MATH
    case "_"  => ???
    case "+"  => ???
    case "-"  => ???
    case "*"  => ???
    case "/"  => ???
    case "%"  => ???
    case "/%" => ???
    case "^"  => ???

    // ITR
    case "len" => len
    case ","   => wrap$
    case ",,"  => wrap
    case ",_"  => unwrap
    case ",,_" => unwrap$
    case "map" => map

    // CONSTANTS
    case "UN" => env.push(UN)
    case "()" => env.push(UN.toFN(env))
    case "[]" => env.push(UN.toARR)
    case "{}" => env.push(UN.toMAP)

    // MAGIC DOT
    case "." => dot

    case _ => throw LinERR(env.code.p, "FN", s"unknown fn \"$x\"")
