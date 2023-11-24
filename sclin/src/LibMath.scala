package sclin

import scala.collection.immutable.VectorMap
import scala.util.chaining.*
import spire.implicits.*
import spire.math.*
import ANY.*

extension (env: ENV)

  def scale: ENV = env.push(NUM(10)).swap.pow.mul
  def trunc: ENV = env.num1(_.toBigInt.toDouble, _.toBigInt)
  def floor: ENV = env.num1(_.floor, _.floor)
  def round: ENV = env.num1(_.round, _.round)
  def ceil: ENV  = env.num1(_.ceil, _.ceil)

  def fromDec: ENV =
    env.num2a((x, y) => Util.fromDec(x.toSafeLong, y.toInt).map(Real(_)))
  def toDec: ENV = env.mod2: (x, y) =>
    val x1 = x.toARR.x.map(_.toNUM.x.toSafeLong)
    y.num1(n => Util.toDec(x1, n.toInt).toDouble, n => Util.toDec(x1, n.toInt))
  def toNumDen: ENV = env.vec1: x =>
    val a = x.toNUM.x.toRational
    Vector(a.numerator, a.denominator).map(NUM(_)).toARR
  def isInt: ENV = env.vec1(_.toNUM.x.isWhole.boolTF)
  def isExact: ENV = env.vec1: x =>
    TF:
      x.toNUM.x match
        case Real.Exact(_) => true
        case _             => false

  def neg: ENV   = env.num1(-_, -_)
  def neg$ : ENV = env.str1(_.reverse)
  def neg$$ : ENV =
    def loop(t: ANY): ANY = t match
      case Lsy(x) => x.reverse.mSEQ(t)
      case ARR(x) => x.reverse.toARR
      case _: MAP => loop(t.toSEQ).toMAP
      case _      => t.str1(_.reverse)
    env.mod1(loop)

  def add: ENV    = env.num2(_ + _, _ + _)
  def add$ : ENV  = env.str2(_ ++ _)
  def add$$ : ENV = env.mod2(_ add$$ _)
  def cons: ENV   = env.mod2(_ cons _)
  def snoc: ENV   = env.mod2(_ snoc _)
  def uncons: ENV = env.mods1(x => Vector(x.get(NUM(0)), x.drop(1)))
  def unsnoc: ENV = env.mods1(x => Vector(x.drop(-1), x.get(NUM(-1))))

  def sub: ENV    = env.num2(_ - _, _ - _)
  def sub$ : ENV  = env.str2((x, y) => x.filterNot(y.contains))
  def sub$$ : ENV = env.mod2(_ sub$$ _)

  def mul: ENV    = env.num2(_ * _, _ * _)
  def mul$ : ENV  = env.strnum(_ * _.intValue)
  def mul$$ : ENV = env.mod2(_ mul$$ _)

  def div: ENV = env.num2(
    _ / _,
    (x, y) => if y == 0 then throw ArithmeticException() else x / y,
    "bad /"
  )
  def divi: ENV = env.num2(
    _.fquot(_),
    (x, y) => if y == 0 then throw ArithmeticException() else x.fquot(y),
    "bad /~"
  )
  def div$ : ENV  = env.strnumq((x, y) => x.grouped(y.intValue).to(LazyList))
  def div$$ : ENV = env.mod2((x, y) => y.vec1(x div$$ _.toInt))

  def mod: ENV = env.num2(
    _.fmod(_),
    (x, y) => if y == 0 then throw ArithmeticException() else x.fmod(y),
    "bad %"
  )
  def divmod: ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).divi.pushs(Vector(x, y)).mod
  def mod$ : ENV  = env.strnumq((x, y) => x.sliding(y.intValue).to(LazyList))
  def mod$$ : ENV = env.mod2((x, y) => y.vec1(x mod$$ _.toInt))
  def mod2$$ : ENV =
    env.mod3((x, y, z) => y.vec2(z)((n, k) => x.mod$$(n.toInt, k.toInt)))

  def pow: ENV = env.num2(
    _.fpow(_),
    (x, y) =>
      if x < 0 && y.reciprocal.fmod(2) == 0 then throw ArithmeticException()
      else x.fpow(y),
    "bad ^"
  )
  def powi: ENV = env.num2(_ ** _.intValue, _ ** _.intValue)
  def pow$ : ENV = env.vec2: (x, y) =>
    Util.cPow(x.toSTR.toSEQ.x, y.toInt).map(_.toARR.toSTR).toSEQ
  def pow$$ : ENV = env.mod2: (x, y) =>
    y.vec1(n => Util.cPow(x.toSEQ.x, n.toInt).map(_.toARR.matchType(x)).toSEQ)

  def exp: ENV = env.num1(_.exp, _.exp)
  def abs: ENV = env.num1(_.abs, _.abs)

  def sin: ENV = env.num1(math.sin, Real.sin)
  def cos: ENV = env.num1(math.cos, Real.cos)
  def tan: ENV = env.num1(
    math.tan,
    x =>
      val d = Real.cos(x)
      if d == 0 then throw ArithmeticException() else Real.sin(x) / d
    ,
    "bad tan"
  )
  def asin: ENV  = env.num1(math.asin, Real.asin)
  def acos: ENV  = env.num1(math.acos, Real.acos)
  def atan: ENV  = env.num1(math.atan, Real.atan)
  def atan2: ENV = env.num2(math.atan2, Real.atan2)
  def sinh: ENV  = env.num1(math.sinh, Real.sinh)
  def cosh: ENV  = env.num1(math.cosh, Real.cosh)
  def tanh: ENV  = env.num1(math.tanh, Real.tanh)
  def asinh: ENV = env.num1(Util.asinh, Real.asinh)
  def acosh: ENV = env.num1(Util.acosh, Real.acosh)
  def atanh: ENV = env.num1(Util.atanh, Real.atanh)

  def ln: ENV =
    env.num1(
      math.log,
      x => if x <= 0 then throw ArithmeticException() else Real.log(x),
      "bad log"
    )
  def log: ENV   = env.arg2((x, y, env) => env.push(x).ln.push(y).ln.div)
  def log10: ENV = env.push(NUM(10)).log

  def isPrime: ENV = env.vec1(_.toNUM.x.toSafeLong.pipe(prime.isPrime).boolTF)
  def factor: ENV = env.vec1:
    _.toNUM.x.toSafeLong
      .pipe(prime.factor)
      .to(VectorMap)
      .map:
        case (x, y) => (NUM(x), NUM(y))
      .toMAP
      .sortBy((a, _) => a, a => a)

  def not: ENV    = env.vec1(_.toBool.pipe(!_).boolTF)
  def not$$ : ENV = env.mod1(_.toBool.pipe(!_).boolTF)
  def min: ENV    = env.vec2((x, y) => if x.cmp(y) < 0 then x else y)
  def and: ENV    = env.vec2((x, y) => (x.toBool && y.toBool).boolTF)
  def min$$ : ENV = env.mod2((x, y) => if x.cmp(y) < 0 then x else y)
  def and$$ : ENV = env.mod2((x, y) => (x.toBool && y.toBool).boolTF)
  def max: ENV    = env.vec2((x, y) => if x.cmp(y) > 0 then x else y)
  def or: ENV     = env.vec2((x, y) => (x.toBool || y.toBool).boolTF)
  def max$$ : ENV = env.mod2((x, y) => if x.cmp(y) > 0 then x else y)
  def or$$ : ENV  = env.mod2((x, y) => (x.toBool || y.toBool).boolTF)

  def cmp: ENV    = env.vec2((x, y) => NUM(x.cmp(y)))
  def cmp$$ : ENV = env.mod2((x, y) => NUM(x.cmp(y)))
  def lt: ENV     = cmp.push(NUM(-1)).eql
  def lt$$ : ENV  = cmp$$.push(NUM(-1)).eql
  def gt: ENV     = cmp.push(NUM(1)).eql
  def gt$$ : ENV  = cmp$$.push(NUM(1)).eql
  def lteq: ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).lt.pushs(Vector(x, y)).eql.or
  def lteq$$ : ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).lt$$.pushs(Vector(x, y)).eql$$.or
  def gteq: ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).gt.pushs(Vector(x, y)).eql.or
  def gteq$$ : ENV = env.arg2: (x, y, env) =>
    env.pushs(Vector(x, y)).gt$$.pushs(Vector(x, y)).eql$$.or
  def eql: ENV     = env.vec2(_.eql(_).boolTF)
  def eql$$ : ENV  = env.mod2(_.eql(_).boolTF)
  def eqls: ENV    = env.vec2(_.eqls(_).boolTF)
  def eqls$$ : ENV = env.mod2(_.eqls(_).boolTF)
  def neq: ENV     = eql.not
  def neq$$ : ENV  = eql$$.not
  def neqs: ENV    = eqls.not
  def neqs$$ : ENV = eqls$$.not

  def bhelp1(f: Long => Long, g: SafeLong => NUMF): ENV =
    env.num1(_.toLong.pipe(f).toDouble, _.toSafeLong.pipe(g))
  def bhelp2(f: (Long, Long) => Long, g: (SafeLong, SafeLong) => NUMF): ENV =
    env.num2(
      (a, b) => f(a.toLong, b.toLong).toDouble,
      (a, b) => g(a.toSafeLong, b.toSafeLong)
    )
  def bNOT: ENV  = bhelp1(~_, ~_.toBigInt)
  def bAND: ENV  = bhelp2(_ & _, _ & _)
  def bOR: ENV   = bhelp2(_ | _, _ | _)
  def bXOR: ENV  = bhelp2(_ ^ _, _ ^ _)
  def bLSH: ENV  = bhelp2(_ << _, _ << _.toInt)
  def bRSH: ENV  = bhelp2(_ >> _, _ >> _.toInt)
  def bURSH: ENV = bhelp2(_ >>> _, _.toLong >>> _.toLong)
