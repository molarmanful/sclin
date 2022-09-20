import org.apfloat.{ApfloatMath => Ap, FixedPrecisionApfloatHelper => Afp, _}
import scala.util.chaining._
import ANY._
import NUMF._

extension (env: ENV)

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

  def evalA(x: ARRW, f: ANY): ANY = env.modStack(_ => x :+ f).eval.getStack(0)

  def quar: ENV =
    env.arg1((x, env) => x.vec1(env.push(_).eval.getStack(0)).pipe(env.push))

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

  def evalLine: ENV = env.arg1((x, env) =>
    var env1 = env
    x.vec1(n =>
      env1 = env1.loadLine(n.toI)
      UN
    )
    env1
  )

  def evalLRel: ENV  = env.push(NUM(env.code.p.l)).add.evalLine
  def evalLHere: ENV = env.push(NUM(0)).evalLine
  def evalLNext: ENV = env.push(NUM(env.code.p.l + 1)).evalLine
  def evalLPrev: ENV = env.push(NUM(env.code.p.l - 1)).evalLine

  def startARR: ENV = env.setArr(env.stack :: env.arr).clr

  def endARR: ENV = env.arr match
    case List()  => env
    case x :: xs => env.setArr(xs).modStack(_ => x).push(env.stack.toARR)

  def endMAP: ENV = endARR.toMAP

  def getType: ENV = env.mod1(_.getType.pipe(STR.apply))

  def toSEQ: ENV = env.mod1(_.toSEQ)
  def toARR: ENV = env.mod1(_.toARR)
  def toMAP: ENV = env.mod1(_.toMAP)
  def toSTR: ENV = env.mod1(_.toSTR)
  def toNUM: ENV = env.mod1(_.toNUM)
  def toFN: ENV  = env.mod1(_.toFN(env))
  def toERR: ENV =
    env.mod2((x, y) => ERR(LinERR(env.code.p, y.toString, x.toString)))

  def toBool: ENV = env.mod1(_.toBool.boolNUM)

  def out: ENV  = env.arg1((x, env) => env.tap(_ => print(x)))
  def outn: ENV = env.arg1((x, env) => env.tap(_ => println(x)))

  def form: ENV = env.mod1(_.toForm.pipe(STR.apply))
  def outf: ENV = env.form.outn

  def dup: ENV  = env.mods1(x => Vector(x, x))
  def dups: ENV = env.push(env.stack.toARR)
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

  def wrap$ : ENV   = env.modx(2, _.toARR)
  def wrap: ENV     = env.modx(1, _.toARR)
  def unwrap: ENV   = env.mods1(_.toARR.x)
  def unwrap$ : ENV = env.arg1((x, env) => env.modStack(_ => x.toARR.x))
  def wrapFN: ENV   = env.wrap.mod1(_.toFN(env))
  def wrapFN$ : ENV = env.wrap$.mod1(_.toFN(env))

  def prec: ENV =
    env.arg1((x, env) => env.copy(fixp = Afp(x.toNUM.x.longValue)))
  def infprec: ENV = env.copy(fixp = Afp(Long.MaxValue))
  def trunc: ENV   = env.num1(_.truncate)
  def floor: ENV   = env.num1(_.floor)
  def round: ENV =
    env.num1(x => env.fixp.divide(env.fixp.multiply(x, 2).floor, 2).ceil)
  def ceil: ENV = env.num1(_.ceil)

  def neg: ENV  = env.num1(env.fixp.negate)
  def add: ENV  = env.num2(env.fixp.add)
  def sub: ENV  = env.num2(env.fixp.subtract)
  def mul: ENV  = env.num2(env.fixp.multiply)
  def div: ENV  = env.num2(env.fixp.divide)
  def divi: ENV = env.num2((x, y) => x.truncate.divide(y.truncate))
  def mod: ENV = env.num2((x, y) =>
    val a = x.truncate
    val b = y.truncate
    a.mod(b).add(b).mod(b)
  )
  def divmod: ENV =
    env.arg2((x, y, env) =>
      env.pushs(Vector(x, y)).divi.pushs(Vector(x, y)).mod
    )
  def pow: ENV  = env.num2(env.fixp.pow)
  def powi: ENV = env.num2((x, y) => env.fixp.pow(x, y.longValue))
  def exp: ENV  = env.num1(env.fixp.exp(_))
  def abs: ENV  = env.num1(Ap.abs)

  def sin: ENV   = env.num1(env.fixp.sin)
  def cos: ENV   = env.num1(env.fixp.cos)
  def tan: ENV   = env.num1(env.fixp.tan)
  def asin: ENV  = env.num1(env.fixp.asin)
  def acos: ENV  = env.num1(env.fixp.acos)
  def atan: ENV  = env.num1(env.fixp.atan)
  def atan2: ENV = env.num2(env.fixp.atan2)
  def sinh: ENV  = env.num1(env.fixp.sinh)
  def cosh: ENV  = env.num1(env.fixp.cosh)
  def tanh: ENV  = env.num1(env.fixp.tanh)
  def asinh: ENV = env.num1(env.fixp.asinh)
  def acosh: ENV = env.num1(env.fixp.acosh)
  def atanh: ENV = env.num1(env.fixp.atanh)

  def log: ENV   = env.num2(env.fixp.log(_, _))
  def ln: ENV    = env.num1(env.fixp.log(_))
  def log10: ENV = env.num1(env.fixp.log(_, 10))

  def map: ENV =
    env.mod2((x, y) => y.vec1(f => x.map(a => env.evalA(Vector(a), f))))
  def zip: ENV =
    env.mod3((x, y, z) =>
      z.vec1(f => x.zip(y)((a, b) => env.evalA(Vector(a, b), f)))
    )

  def dot: ENV = ???

  def cmd(x: String): ENV = x match

    // TYPES
    case "type" => getType
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
    case ">!"   => toBool
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
    case "\\"  => wrapFN
    case ",\\" => wrapFN$
    case "#"   => eval
    case "Q"   => quar
    case "@@"  => evalLine
    case "@~"  => evalLRel
    case "@"   => evalLHere
    case ";"   => evalLNext
    case ";;"  => evalLPrev

    // NUM/MATH
    case ">~"   => prec
    case "oo>~" => infprec
    case "I"    => trunc
    case "|_"   => floor
    case "|-"   => round
    case "|^"   => ceil
    case "_"    => neg
    case "+"    => add
    case "-"    => sub
    case "*"    => mul
    case "/"    => div
    case "/~"   => divi
    case "%"    => mod
    case "/%"   => divmod
    case "^"    => pow
    case "^~"   => powi
    case "e^"   => exp
    case "abs"  => abs

    // NUM/TRIG
    case "sin"    => sin
    case "cos"    => cos
    case "tan"    => tan
    case "sin_"   => asin
    case "cos_"   => acos
    case "tan_"   => atan
    case "tan_II" => atan2
    case "sinh"   => sinh
    case "cosh"   => cosh
    case "tanh"   => tanh
    case "sinh_"  => asinh
    case "cosh_"  => acosh
    case "tanh_"  => atanh

    // NUM/LOG
    case "log"  => log
    case "ln"   => ln
    case "logX" => log10

    // ITR
    case "len" => len
    case ","   => wrap$
    case ",,"  => wrap
    case ",_"  => unwrap
    case ",,_" => unwrap$
    case "map" => map
    case "zip" => zip

    // CONSTANTS
    case "UN"  => env.push(UN)
    case "()"  => env.push(UN.toFN(env))
    case "[]"  => env.push(UN.toARR)
    case "{}"  => env.push(UN.toMAP)
    case "$PI" => env.push(NUM(env.fixp.pi))
    case "$E"  => env.push(NUM(env.fixp.exp(1)))

    // MAGIC DOT
    case "." => dot

    case _ => throw LinERR(env.code.p, "FN", s"unknown fn \"$x\"")
