import scala.collection.immutable.VectorMap
import scala.io.StdIn._
import scala.util.chaining._
import spire.algebra._
import spire.implicits._
import spire.math._
import ANY._

extension (env: ENV)

  def eval: ENV = env.arg1((x, env) =>
    x match
      case f: FN =>
        val env1 = env.copy(code = f)
        env.code.x match
          case List() => env1
          case _      => env.modStack(_ => env1.exec.stack)
      case f: CMD => env.execA(f)
      case _      => env.push(x).toFN.eval
  )
  def evale: ENV = env.arg1((x, env) =>
    x match
      case f: FN => env.modStack(_ => env.copy(code = f).exec.stack)
      case _     => env.push(x).eval
  )
  def evalA1(x: ARRW[ANY], f: ANY): ANY =
    env.modStack(_ => x :+ f).evale.getStack(0)
  def evalA2(x: ARRW[ANY], f: ANY): (ANY, ANY) =
    val env1 = env.modStack(_ => x :+ f).evale
    (env1.getStack(1), env1.getStack(0))
  def quar: ENV =
    env.arg1((x, env) => x.vec1(env.push(_).evale.getStack(0)).pipe(env.push))

  def startFN: ENV =
    def loop(
        code: List[ANY],
        d: Int = 1,
        res: List[ANY] = List()
    ): (List[ANY], List[ANY]) =
      if d > 0 then
        code match
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
          case _ => (code, res)
      else (code, res)
    val (code, res) = loop(env.code.x)
    val (cs, c) = res match
      case cs :+ c => (cs, c)
      case _       => (res, CMD(")"))
    env.modCode(_ => code).pushs(Vector(FN(env.code.p, cs), c)).eval

  def evalLine: ENV = env.arg1((x, env) =>
    val i    = x.toI
    val env1 = env.fnLine(i)
    env1.push(env1.getLineF(i)).eval
  )
  def getLNum: ENV = env.push(NUM(env.code.p.l))
  def getLFile: ENV = env.push(env.code.p.f match
    case Some(x) => STR(x.toString)
    case _       => UN
  )
  def evalLRel: ENV  = getLNum.add.evalLine
  def evalLHere: ENV = env.push(NUM(0)).evalLRel
  def evalLNext: ENV = env.push(NUM(1)).evalLRel
  def evalLPrev: ENV = env.push(NUM(-1)).evalLRel
  def getLn: ENV     = env.mod1(n => env.getLineS(n.toNUM.x.intValue))
  def getLRel: ENV   = getLNum.add.getLn
  def getLHere: ENV  = env.push(NUM(0)).getLRel
  def getLNext: ENV  = env.push(NUM(1)).getLRel
  def getLPrev: ENV  = env.push(NUM(-1)).getLRel
  def evalAnd: ENV =
    env.arg2((x, f, env) => if x.toBool then env.push(f).eval else env)
  def evalOr: ENV =
    env.arg2((x, f, env) => if x.toBool then env else env.push(f).eval)
  def evalIf: ENV =
    env.arg3((x, f, g, env) => env.push(if x.toBool then f else g).eval)
  def evalTimes: ENV =
    env.arg2((f, n, env) =>
      def loop(env: ENV, n: NUMF): ENV =
        if n.compare(0) > 0 then loop(env.push(f).evale, n - 1)
        else env
      loop(env, n.toNUM.x)
    )
  def evalTry: ENV = env.arg2((f, g, env) =>
    try env.push(f).evale
    catch
      case e: LinERR => env.pushs(Vector(ERR(e), g)).evale
      case e: LinEx  => env.pushs(Vector(ERR(e.toERR(env)), g)).evale
      case e =>
        env.pushs(Vector(ERR(LinEx("_", e.getMessage).toERR(env)), g)).evale
  )
  def throwERR: ENV = env.arg1((x, env) =>
    x match
      case ERR(x) => throw x
      case _      => throw LinEx("_", x.toString).toERR(env)
  )
  def evalArrSt: ENV = env.arg2((x, f, env) =>
    env.push(env.push(x).unwrap$.push(f).evale.stack.toARR)
  )
  def evalStArr: ENV = env.arg1((f, env) => env.wrap$$.push(f).quar.unwrap$)

  def startARR: ENV = env.setArr(env.stack :: env.arr).clr
  def endARR: ENV = env.arr match
    case List()  => env
    case x :: xs => env.setArr(xs).modStack(_ => x).push(env.stack.toARR)
  def endMAP: ENV = endARR.toMAP

  def getType: ENV = env.mod1(_.getType.pipe(STR.apply))
  def toSEQ: ENV   = env.mod1(_.toSEQ)
  def toARR: ENV   = env.mod1(_.toARR)
  def toMAP: ENV   = env.mod1(_.toMAP)
  def toSTR: ENV   = env.mod1(_.toSTR)
  def toNUM: ENV   = env.mod1(_.toNUM)
  def toFN: ENV    = env.mod1(_.toFN(env))
  def toERR: ENV =
    env.mod2((x, y) => ERR(LinERR(env.code.p, y.toString, x.toString)))
  def toBool: ENV = env.mod1(_.toBool.boolNUM)

  def in: ENV   = env.push(STR(readLine))
  def out: ENV  = env.arg1((x, env) => env.tap(_ => print(x)))
  def outn: ENV = env.arg1((x, env) => env.tap(_ => println(x)))

  def form: ENV = env.mod1(_.toForm.pipe(STR.apply))
  def outf: ENV = env.form.outn

  def dup: ENV  = env.mods1(x => Vector(x, x))
  def dups: ENV = env.push(env.stack.toARR)
  def over: ENV = env.mods2((x, y) => Vector(x, y, x))
  def pick: ENV =
    env.arg1((x, env) => env.push(x.vec1(n => env.getStack(n.toI))))

  def pop: ENV = env.mods1(_ => Vector())
  def clr: ENV = env.modStack(_ => Vector())
  def nip: ENV = env.mod2((x, _) => x)
  def nix: ENV = env.arg1((x, env) =>
    env.modStack(s =>
      val i = env.iStack(x.toI)
      if 0 < i && i < s.length then s.patch(i, Nil, 1) else s
    )
  )

  def swap: ENV = env.mods2((x, y) => Vector(y, x))
  def rev: ENV  = env.modStack(_.reverse)
  def tuck: ENV = env.mods2((x, y) => Vector(y, x, y))
  def trade: ENV =
    env.arg1((x, env) => env.push(x).rollu.push(x).push(NUM(1)).sub.roll)

  def rot: ENV  = env.mods3((x, y, z) => Vector(y, z, x))
  def rotu: ENV = env.mods3((x, y, z) => Vector(z, x, y))
  def roll: ENV =
    env.arg1((x, env) => env.push(x).pick.push(x).push(NUM(1)).add.nix)
  def rollu: ENV = env.arg1((x, env) =>
    val a = env.getStack(0)
    env.modStack(s => s.patch(env.iStack(x.toI), Vector(a), 0)).pop
  )

  def dip: ENV = env.arg1((x, env) => env.evale.push(x))

  def get: ENV    = env.mod2((x, y) => y.vec1(x.get(_)))
  def get$$ : ENV = env.mod2(_.get(_))

  def len: ENV = env.mod1(x => NUM(x.length))

  def rep: ENV = env.mod1(x => LazyList.continually(x).toSEQ)
  def cyc: ENV = env.mod1(x => LazyList.continually(x).toSEQ.flat)
  def itr: ENV = env.mod2((x, y) =>
    y.vec1(f => LazyList.iterate(x)(s => env.evalA1(Vector(s), f)).toSEQ)
  )
  def unfold: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      LazyList
        .unfold(x)(s =>
          env.modStack(_ => Vector(s, f)).evale.stack match
            case st :+ n =>
              st match
                case _ :+ m => Some(m, n)
                case _      => throw LinEx("ST_LEN", "stack length = 1")
            case _ => None
        )
        .toSEQ
    )
  )

  def enumL: ENV = env.mod1 {
    case x: MAP => x.toSEQ
    case x      => LazyList.from(0).map(NUM(_)).toSEQ.zip(x, Vector(_, _).toARR)
  }
  def keys: ENV = env.enumL.mod1(_.map(_.get(NUM(0))))
  def vals: ENV = env.enumL.mod1(_.map(_.get(NUM(1))))

  def range: ENV = env.num2a((x, y) =>
    Range(x.intValue, y.intValue, (y - x).compare(0)).map(Real(_))
  )
  def orang: ENV = env.push(NUM(0)).range
  def rango: ENV = env.push(NUM(0)).swap.range
  def irang: ENV = env.push(NUM(1)).range
  def rangi: ENV = env.push(NUM(1)).swap.range

  def shuffle: ENV = env.mod1(_.shuffle)
  def getr: ENV    = env.shuffle.push(NUM(0)).get

  def split: ENV  = env.str2a(_.split(_))
  def ssplit: ENV = env.str1a(_.split(raw"\s"))
  def join: ENV   = env.mod2((x, y) => y.vec1(s => STR(x.join(s.toString))))

  def wrap$ : ENV   = env.modx(2, _.toARR)
  def wrap: ENV     = env.modx(1, _.toARR)
  def wrap$$ : ENV  = env.modStack(x => Vector(x.toARR))
  def unwrap: ENV   = env.mods1(_.toARR.x)
  def unwrap$ : ENV = env.arg1((x, env) => env.modStack(_ => x.toARR.x))
  def wrapFN: ENV   = env.wrap.mod1(_.toFN(env))

  def tk: ENV = env.mod2((x, y) => y.vec1(n => x.take(n.toI)))
  def dp: ENV = env.mod2((x, y) => y.vec1(n => x.drop(n.toI)))

  def scale: ENV = env.push(NUM(10)).swap.pow.mul
  def trunc: ENV = env.num1(_.toBigInt)
  def floor: ENV = env.num1(_.floor)
  def round: ENV = env.num1(_.round)
  def ceil: ENV  = env.num1(_.ceil)

  def neg: ENV   = env.num1(-_)
  def neg$ : ENV = env.str1(_.reverse)
  def neg$$ : ENV =
    def loop(x: ANY): ANY = x match
      case SEQ(x) => x.reverse.toSEQ
      case ARR(x) => x.reverse.toARR
      case _: MAP => loop(x.toSEQ).toMAP
      case _      => x.str1(_.reverse)
    env.mod1(loop)

  def add: ENV   = env.num2(_ + _)
  def add$ : ENV = env.str2(_ ++ _)
  def add$$ : ENV =
    def loop(x: ANY, y: ANY): ANY = (x, y) match
      case (It(x), SEQ(y))      => SEQ(x.toSEQ.x #::: y)
      case (ARR(x), ARR(y))     => ARR(x ++ y)
      case (MAP(x), MAP(y))     => MAP(x ++ y)
      case (FN(p, x), FN(_, y)) => FN(p, x ++ y)
      case (FN(p, _), It(y))    => loop(x.toARR, y).pFN(p)
      case (SEQ(x), ARR(y))     => SEQ(x :++ y)
      case (_, SEQ(y))          => SEQ(x #:: y)
      case (SEQ(x), _)          => SEQ(x :+ y)
      case (ARR(x), _)          => ARR(x :+ y)
      case (_, ARR(y))          => ARR(x +: y)
      case (FN(p, x), _)        => FN(p, x :+ y)
      case (_, FN(p, y))        => FN(p, x +: y)
      case _                    => loop(Vector(x).toARR, y)
    env.mod2(loop)

  def sub: ENV   = env.num2(_ - _)
  def sub$ : ENV = env.str2(_.replace(_, ""))
  def sub$$ : ENV =
    def loop(x: ANY, y: ANY): ANY = (x, y) match
      case (SEQ(x), y: SEQ) => x.filterNot(y.has).toSEQ
      case (ARR(x), It(y))  => x.filterNot(y.has).toARR
      case (MAP(x), MAP(y)) => y.foldLeft(x)(_ - _._1).toMAP
      case (MAP(x), It(y))  => y.foldLeft(x)(_ - _).toMAP
      case (It(x), MAP(y))  => loop(x, y.keys.toARR)
      case (It(x), It(y))   => loop(x, y.toSEQ)
      case (Itr(x), _)      => loop(x, Vector(y).toARR)
      case (FN(p, _), _)    => loop(x.toARR, y).pFN(p)
      case _                => loop(Vector(x).toARR, y)
    env.mod2(loop)

  def mul: ENV   = env.num2(_ * _)
  def mul$ : ENV = env.strnum(_ * _.intValue)
  def mul$$ : ENV =
    def loop(x: ANY, y: ANY): ANY = (x, y) match
      case (Itr(x), Itr(y)) => x.zip(y, loop).flat
      case (x: SEQ, _)      => LazyList.fill(y.toI)(x).toSEQ.flat
      case (x: ARR, _)      => Vector.fill(y.toI)(x).toARR.flat
      case _                => loop(Vector(x).toARR, y)
    env.mod2(loop)

  def div: ENV = env.num2(
    (x, y) => if y == 0 then throw ArithmeticException() else x / y,
    "bad /"
  )
  def divi: ENV = env.num2(
    (x, y) => if y == 0 then throw ArithmeticException() else x.fquot(y),
    "bad /~"
  )
  def div$ : ENV = env.strnuma((x, y) => x.grouped(y.intValue))
  def div$$ : ENV =
    def loop(x: ANY, y: ANY): ANY = (x, y) match
      case (SEQ(x), _)   => x.grouped(y.toNUM.toI).map(_.toSEQ).toSEQ
      case (ARR(x), _)   => x.grouped(y.toNUM.toI).map(_.toARR).toSEQ
      case (MAP(x), _)   => x.grouped(y.toNUM.toI).map(_.toMAP).toSEQ
      case (FN(p, x), _) => x.grouped(y.toNUM.toI).map(_.pFN(p)).pFN(p)
      case _             => loop(Vector(x).toARR, y)
    env.mod2((x, y) => y.vec1(loop(x, _)))

  def mod: ENV = env.num2(_ fmod _)
  def divmod: ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).divi.pushs(Vector(x, y)).mod
  )
  def mod$ : ENV = env.strnuma((x, y) => x.sliding(y.intValue))
  def mod$$ : ENV =
    def loop(x: ANY, y: ANY): ANY = (x, y) match
      case (SEQ(x), _)   => x.sliding(y.toNUM.toI).map(_.toSEQ).toSEQ
      case (ARR(x), _)   => x.sliding(y.toNUM.toI).map(_.toARR).toSEQ
      case (MAP(x), _)   => x.sliding(y.toNUM.toI).map(_.toMAP).toSEQ
      case (FN(p, x), _) => x.sliding(y.toNUM.toI).map(_.pFN(p)).pFN(p)
      case _             => loop(Vector(x).toARR, y)
    env.mod2((x, y) => y.vec1(loop(x, _)))

  def pow: ENV  = env.num2(_ fpow _, "bad ^")
  def powi: ENV = env.num2(_ ** _.intValue)

  def exp: ENV = env.num1(_.exp)
  def abs: ENV = env.num1(_.abs)

  def sin: ENV = env.num1(Real.sin)
  def cos: ENV = env.num1(Real.cos)
  def tan: ENV = env.num1(
    x =>
      val d = Real.cos(x)
      if d == 0 then throw ArithmeticException() else Real.sin(x) / d
    ,
    "bad tan"
  )
  def asin: ENV  = env.num1(Real.asin)
  def acos: ENV  = env.num1(Real.acos)
  def atan: ENV  = env.num1(Real.atan)
  def atan2: ENV = env.num2(Real.atan2)
  def sinh: ENV  = env.num1(Real.sinh)
  def cosh: ENV  = env.num1(Real.cosh)
  def tanh: ENV  = env.num1(Real.tanh)
  def asinh: ENV = env.num1(Real.asinh)
  def acosh: ENV = env.num1(Real.acosh)
  def atanh: ENV = env.num1(Real.atanh)

  def ln: ENV    = env.num1(Real.log)
  def log: ENV   = env.arg2((x, y, env) => env.push(x).ln.push(y).ln.div)
  def log10: ENV = env.push(NUM(10)).log

  def isPrime: ENV = env.num1(x => prime.isPrime(x.toSafeLong).boolI)
  def factor: ENV = env.vec1(
    _.toNUM.x.toSafeLong
      .pipe(prime.factor)
      .to(VectorMap)
      .map { case (x, y) => (NUM(x), NUM(y)) }
      .toMAP
  )

  def not: ENV    = env.vec1(_.toBool.unary_!.boolNUM)
  def not$$ : ENV = env.mod1(_.toBool.unary_!.boolNUM)
  def min: ENV    = env.vec2((x, y) => if x.cmp(y) < 0 then x else y)
  def and: ENV    = env.vec2((x, y) => (x.toBool && y.toBool).boolNUM)
  def and$$ : ENV = env.mod2((x, y) => (x.toBool && y.toBool).boolNUM)
  def max: ENV    = env.vec2((x, y) => if x.cmp(y) > 0 then x else y)
  def or: ENV     = env.vec2((x, y) => (x.toBool || y.toBool).boolNUM)
  def or$$ : ENV  = env.mod2((x, y) => (x.toBool || y.toBool).boolNUM)

  def cmp: ENV    = env.vec2((x, y) => NUM(x.cmp(y)))
  def cmp$$ : ENV = env.mod2((x, y) => NUM(x.cmp(y)))
  def lt: ENV     = cmp.push(NUM(-1)).eql
  def lt$$ : ENV  = cmp$$.push(NUM(-1)).eql
  def gt: ENV     = cmp.push(NUM(1)).eql
  def gt$$ : ENV  = cmp$$.push(NUM(1)).eql
  def lteq: ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).lt.pushs(Vector(x, y)).eql.or
  )
  def lteq$$ : ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).lt$$.pushs(Vector(x, y)).eql$$.or
  )
  def gteq: ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).gt.pushs(Vector(x, y)).eql.or
  )
  def gteq$$ : ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).gt$$.pushs(Vector(x, y)).eql$$.or
  )
  def eql: ENV    = env.vec2(_.eql(_).boolNUM)
  def eql$$ : ENV = env.mod2(_.eql(_).boolNUM)
  def neq: ENV    = eql.not
  def neq$$ : ENV = eql$$.not

  def map: ENV = env.mod2((x, y) =>
    x match
      case x: MAP => y.vec1(f => x.mapM((a, b) => env.evalA2(Vector(a, b), f)))
      case _      => y.vec1(f => x.map(a => env.evalA1(Vector(a), f)))
  )
  def flatMap: ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f => x.flatMapM((a, b) => env.evalA1(Vector(a, b), f)))
      case _ => y.vec1(f => x.flatMap(a => env.evalA1(Vector(a), f)))
  )
  def flat: ENV = env.mod1(_.flat)
  def zip: ENV = env.mod3((x, y, z) =>
    z.vec1(f => x.zip(y, (a, b) => env.evalA1(Vector(a, b), f)))
  )

  def fold: ENV = env.mod3((x, y, z) =>
    x match
      case x: MAP =>
        z.vec1(f =>
          x.foldLeftM(y)((a, b) => env.evalA1(Vector(b._1, a, b._2), f))
        )
      case _ => y.vec1(f => x.map(a => env.evalA1(Vector(a), f)))
    z.vec1(f => x.foldLeft(y)((a, b) => env.evalA1(Vector(a, b), f)))
  )

  def fltr: ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f => x.filterM((a, b) => env.evalA1(Vector(a, b), f).toBool))
      case _ => y.vec1(f => x.filter(a => env.evalA1(Vector(a), f).toBool))
  )

  def any: ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f =>
          x.anyM((a, b) => env.evalA1(Vector(a, b), f).toBool).boolNUM
        )
      case _ => y.vec1(f => x.any(a => env.evalA1(Vector(a), f).toBool).boolNUM)
  )
  def all: ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f =>
          x.allM((a, b) => env.evalA1(Vector(a, b), f).toBool).boolNUM
        )
      case _ => y.vec1(f => x.all(a => env.evalA1(Vector(a), f).toBool).boolNUM)
  )

  def tkwl: ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f => x.takeWhileM((a, b) => env.evalA1(Vector(a, b), f).toBool))
      case _ => y.vec1(f => x.takeWhile(a => env.evalA1(Vector(a), f).toBool))
  )
  def dpwl: ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f => x.dropWhileM((a, b) => env.evalA1(Vector(a, b), f).toBool))
      case _ => y.vec1(f => x.dropWhile(a => env.evalA1(Vector(a), f).toBool))
  )

  def find: ENV = env.mod2((x, y) =>
    y.vec1(f => x.find(a => env.evalA1(Vector(a), f).toBool).getOrElse(UN))
    x match
      case x: MAP =>
        y.vec1(f =>
          x.findM((a, b) => env.evalA1(Vector(a, b), f).toBool)
            .map { case (a, b) => Vector(a, b).toARR }
            .getOrElse(UN)
        )
      case _ => y.vec1(f => x.dropWhile(a => env.evalA1(Vector(a), f).toBool))
  )

  def uniq: ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f => x.uniqByM((a, b) => env.evalA1(Vector(a, b), f)))
      case _ => y.vec1(f => x.uniqBy(a => env.evalA1(Vector(a), f)))
  )
  def sort: ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f => x.sortByM((a, b) => env.evalA1(Vector(a, b), f)))
      case _ => y.vec1(f => x.sortBy(a => env.evalA1(Vector(a), f)))
  )
  def sort$ : ENV = env.mod2((x, y) =>
    x match
      case x: MAP =>
        y.vec1(f =>
          x.sortWithM((i, j, a, b) => env.evalA1(Vector(i, j, a, b), f).toBool)
        )
      case _ =>
        y.vec1(f => x.sortWith((a, b) => env.evalA1(Vector(a, b), f).toBool))
  )

  def dot: ENV =
    env.code.x match
      case c :: cs =>
        env
          .modCode(_ => cs)
          .pipe(env =>
            c match
              case STR(x) => env.push(STR(StringContext.processEscapes(x)))
              case CMD(x) => env.wrapFN.push(CMD(x)).add$$
              case _      => env.push(c)
          )
      case _ => evalLNext

  def cmd(x: String): ENV = x match

    // CMDOC START

    case s"\$k" if k != "" => env.push(CMD(k).toFN(env))
    case s"#$k" if k != "" => env

    case s"$$$$$k" if k != "" && env.gscope.contains(k) =>
      env.push(env.gscope(k))
    case s"=$$$$$k" if k != ""                       => env.arg1((v, env) => env.addGlob(k, v))
    case s"$$$k" if k != "" && env.scope.contains(k) => env.push(env.scope(k))
    case s"=$$$k" if k != ""                         => env.arg1((v, env) => env.addLoc(k, v))

    case x if env.scope.contains(x)  => env.push(env.scope(x)).eval
    case x if env.gscope.contains(x) => env.push(env.gscope(x)).eval

    case "type" => getType
    case "("    => startFN
    case ")"    => env
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
    case ">?"   => toBool
    case "form" => form

    case "UN"   => env.push(UN)
    case "()"   => env.push(UN.toFN(env))
    case "[]"   => env.push(UN.toARR)
    case "{}"   => env.push(UN.toMAP)
    case "$PI"  => env.push(NUM(Real.pi))
    case "$E"   => env.push(NUM(Real.e))
    case "$PHI" => env.push(NUM(Real.phi))
    case "$rng" => env.push(NUM(random))
    case "$L"   => getLNum
    case "$F"   => getLFile
    case "$W"   => env.push(LazyList.from(0).map(NUM(_)).toSEQ)
    case "$P"   => env.push(prime.lazyList.map(NUM(_)).toSEQ)

    case "I>"  => in
    case ">O"  => out
    case "n>O" => outn
    case "f>O" => outf

    case "dup"   => dup
    case "dups"  => dups
    case "over"  => over
    case "pick"  => pick
    case "pop"   => pop
    case "clr"   => clr
    case "nip"   => nip
    case "nix"   => nix
    case "swap"  => swap
    case "rev"   => rev
    case "tuck"  => tuck
    case "trade" => trade
    case "rot"   => rot
    case "rot_"  => rotu
    case "roll"  => roll
    case "roll_" => rollu
    case "dip"   => dip

    case "\\"  => wrapFN
    case "#"   => eval
    case "Q"   => quar
    case "@@"  => evalLine
    case "@~"  => evalLRel
    case "@"   => evalLHere
    case ";"   => evalLNext
    case ";;"  => evalLPrev
    case "g@@" => getLn
    case "g@~" => getLRel
    case "g@"  => getLHere
    case "g;"  => getLNext
    case "g;;" => getLPrev
    case "&#"  => evalAnd
    case "|#"  => evalOr
    case "?#"  => evalIf
    case "*#"  => evalTimes
    case "!#"  => evalTry
    case ">!"  => throwERR
    case "'"   => evalArrSt
    case "'_"  => evalStArr

    case "E"   => scale
    case "I"   => trunc
    case "|_"  => floor
    case "|~"  => round
    case "|^"  => ceil
    case "_"   => neg
    case "__"  => neg$
    case "_`"  => neg$$
    case "+"   => add
    case "++"  => add$
    case "+`"  => add$$
    case "-"   => sub
    case "--"  => sub$
    case "-`"  => sub$$
    case "*"   => mul
    case "**"  => mul$
    case "*`"  => mul$$
    case "/"   => div
    case "/~"  => divi
    case "//"  => div$
    case "/`"  => div$$
    case "%"   => mod
    case "/%"  => divmod
    case "%%"  => mod$
    case "%`"  => mod$$
    case "^"   => pow
    case "^~"  => powi
    case "^^"  => ???
    case "^`"  => ???
    case "e^"  => exp
    case "abs" => abs

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

    case "log"  => log
    case "ln"   => ln
    case "logX" => log10

    case "P?" => isPrime
    case "P/" => factor

    case "!"    => not
    case "!`"   => not$$
    case "&"    => min
    case "&&"   => and
    case "&`"   => and$$
    case "|"    => max
    case "||"   => or
    case "|`"   => or$$
    case "<=>"  => cmp
    case "<=>`" => cmp$$
    case "="    => eql
    case "=`"   => eql$$
    case "!="   => neq
    case "!=`"  => neq$$
    case "<"    => lt
    case "<`"   => lt$$
    case ">"    => gt
    case ">`"   => gt$$
    case "<="   => lteq
    case "<=`"  => gt$$
    case ">="   => gteq
    case ">=`"  => gt$$

    case ":"     => get
    case ":R"    => getr
    case "::"    => ???
    case ":`"    => get$$
    case ":?"    => ???
    case ":?`"   => ???
    case "len"   => len
    case ","     => wrap$
    case ",,"    => wrap
    case ",`"    => wrap$$
    case ",_"    => unwrap
    case ",,_"   => unwrap$
    case "tk"    => tk
    case "dp"    => dp
    case "flat"  => flat
    case "rep"   => rep
    case "cyc"   => cyc
    case "itr"   => itr
    case "fold_" => unfold
    case ">kv"   => enumL
    case ">k"    => keys
    case ">v"    => vals
    case "n>m"   => range
    case "o>n"   => rango
    case "n>o"   => orang
    case "i>n"   => rangi
    case "n>i"   => irang
    case "shuf"  => shuffle

    case "S>c" => ???
    case "c>S" => ???
    case "<>"  => split
    case "<>`" => ???
    case "><"  => join
    case "><`" => ???
    case "c<>" => env.push(STR("")).split
    case "c><" => env.push(STR("")).join
    case "w<>" => env.push(STR(" ")).split
    case "w><" => env.push(STR(" ")).join
    case "n<>" => env.push(STR("\n")).split
    case "n><" => env.push(STR("\n")).join
    case "s<>" => ssplit

    case "map"   => map
    case "zip"   => zip
    case "tbl"   => ???
    case "mapf"  => flatMap
    case "fold"  => fold
    case "fltr"  => fltr
    case "any"   => any
    case "all"   => all
    case "tk*"   => tkwl
    case "dp*"   => dpwl
    case "find"  => find
    case "uniq"  => uniq
    case "sort"  => sort
    case "sort~" => sort$

    case "." => dot

    // CMDOC END

    case _ => throw LinEx("FN", s"unknown fn \"$x\"")
