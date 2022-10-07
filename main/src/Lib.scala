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
  def nip: ENV = env.mod2((_, x) => x)
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

  def has: ENV    = env.mod2((x, y) => y.vec1(x.has(_).boolNUM))
  def has$$ : ENV = env.mod2(_.has(_).boolNUM)

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

  def range: ENV = env.num2q((x, y) =>
    Range(x.intValue, y.intValue, (y - x).compare(0)).iterator.map(Real(_))
  )

  def shuffle: ENV = env.mod1(_.shuffle)
  def getr: ENV    = env.shuffle.push(NUM(0)).get
  def perm: ENV    = env.mod1(_.permutations)
  def comb: ENV    = env.mod2((x, y) => x.combinations(y.toI))
  def powset: ENV = env.dup.len
    .push(NUM(1))
    .add
    .push(NUM(0))
    .swap
    .range
    .mod2((x, y) => y.flatMap(n => x.combinations(n.toI)))

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
    def loop(x: ANY, y: ANY): ANY = x match
      case SEQ(x)   => x.grouped(y.toI).map(_.toSEQ).toSEQ
      case ARR(x)   => x.grouped(y.toI).map(_.toARR).toSEQ
      case MAP(x)   => x.grouped(y.toI).map(_.toMAP).toSEQ
      case FN(p, x) => x.grouped(y.toI).map(_.pFN(p)).pFN(p)
      case _        => loop(Vector(x).toARR, y)
    env.mod2((x, y) => y.vec1(loop(x, _)))

  def mod: ENV = env.num2(
    (x, y) => if y == 0 then throw ArithmeticException() else x.fmod(y),
    "bad %"
  )
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
  def min$$ : ENV = env.mod2((x, y) => if x.cmp(y) < 0 then x else y)
  def and$$ : ENV = env.mod2((x, y) => (x.toBool && y.toBool).boolNUM)
  def max: ENV    = env.vec2((x, y) => if x.cmp(y) > 0 then x else y)
  def or: ENV     = env.vec2((x, y) => (x.toBool || y.toBool).boolNUM)
  def max$$ : ENV = env.mod2((x, y) => if x.cmp(y) > 0 then x else y)
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
    y.vec1(f =>
      x.mapM(
        (a, b) => env.evalA2(Vector(a, b), f),
        a => env.evalA1(Vector(a), f)
      )
    )
  )
  def tapMap: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.mapM(
        (a, b) => env.evalA2(Vector(a, b), f).pipe(_ => (a, b)),
        a => env.evalA1(Vector(a), f).pipe(_ => a)
      )
    )
  )
  def flatMap: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.flatMapM(
        (a, b) => env.evalA1(Vector(a, b), f),
        a => env.evalA1(Vector(a), f)
      )
    )
  )
  def flat: ENV = env.mod1(_.flat)

  def zip: ENV = env.mod3((x, y, z) =>
    z.vec1(f => x.zip(y, (a, b) => env.evalA1(Vector(a, b), f)))
  )
  def tbl: ENV = env.mod3((x, y, z) =>
    z.vec1(f => x.table(y, (a, b) => env.evalA1(Vector(a, b), f)))
  )

  def fold: ENV = env.mod3((x, y, z) =>
    z.vec1(f =>
      x.foldLeftM(y)(
        (a, b) => env.evalA1(Vector(b._1, a, b._2), f),
        (a, b) => env.evalA1(Vector(a, b), f)
      )
    )
  )

  def fltr: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.filterM(
        (a, b) => env.evalA1(Vector(a, b), f).toBool,
        a => env.evalA1(Vector(a), f).toBool
      )
    )
  )

  def any: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.anyM(
        (a, b) => env.evalA1(Vector(a, b), f).toBool,
        a => env.evalA1(Vector(a), f).toBool
      ).boolNUM
    )
  )
  def all: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.allM(
        (a, b) => env.evalA1(Vector(a, b), f).toBool,
        a => env.evalA1(Vector(a), f).toBool
      ).boolNUM
    )
  )

  def tkwl: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.takeWhileM(
        (a, b) => env.evalA1(Vector(a, b), f).toBool,
        a => env.evalA1(Vector(a), f).toBool
      )
    )
  )
  def dpwl: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.dropWhileM(
        (a, b) => env.evalA1(Vector(a, b), f).toBool,
        a => env.evalA1(Vector(a), f).toBool
      )
    )
  )

  def find: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.findM(
        (a, b) => env.evalA1(Vector(a, b), f).toBool,
        a => env.evalA1(Vector(a), f).toBool
      ) match
        case Some((a, b)) => Vector(a, b).toARR
        case Some(a: ANY) => a
        case _            => UN
    )
  )

  def uniq: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.uniqByM(
        (a, b) => env.evalA1(Vector(a, b), f),
        a => env.evalA1(Vector(a), f)
      )
    )
  )
  def sort: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.sortByM(
        (a, b) => env.evalA1(Vector(a, b), f),
        a => env.evalA1(Vector(a), f)
      )
    )
  )
  def sort$ : ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.sortWithM(
        (i, j, a, b) => env.evalA1(Vector(i, j, a, b), f).toBool,
        (a, b) => env.evalA1(Vector(a, b), f).toBool
      )
    )
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

    case "(" => startFN
    case ")" => env
    case "[" => startARR
    case "]" => endARR
    case "{" => startARR
    case "}" => endMAP

    /*
    @s a -> (x STR)
    Type of `a`.
     */
    case "type" => getType
    /*
    @s a -> (x STR)
    `a` as formatted string.
     */
    case "form" => form
    /*
    @s a -> (x STR)
    Converts `a` to `SEQ`.
     */
    case ">Q" => toSEQ
    /*
    @s a -> (x ARR)
    Converts `a` to `ARR`.
     */
    case ">A" => toARR
    /*
    @s a -> (x ARR)
    Converts `a` to `MAP`.
     */
    case ">M" => toMAP
    /*
    @s a -> (x STR)
    Converts `a` to `STR`.
     */
    case ">S" => toSTR
    /*
    @s a -> (x NUM)
    Converts `a` to `NUM`.
     */
    case ">N" => toNUM
    /*
    @s a -> (x FN)
    Converts `a` to `FN`.
     */
    case ">F" => toFN
    /*
    @s (a >STR) (b >STR) -> (x ERR)
    Converts `a` to `ERR` with message `b`.
     */
    case ">E" => toERR
    /*
    @s a -> (x NUM)
    1 or 0 depending on truthiness of `a`.
     */
    case ">?" => toBool

    /*
    @s -> UN
    `UN`
     */
    case "UN" => env.push(UN)
    /*
    @s -> (x FN)
    Empty `FN`.
     */
    case "()" => env.push(UN.toFN(env))
    /*
    @s -> (x ARR)
    Empty `ARR`.
     */
    case "[]" => env.push(UN.toARR)
    /*
    @s -> (x MAP)
    Empty `MAP`.
     */
    case "{}" => env.push(UN.toMAP)
    /*
    @s -> (x NUM)
    π (Pi).
     */
    case "$PI" => env.push(NUM(Real.pi))
    /*
    @s -> (x NUM)
    e (Euler's number).
     */
    case "$E" => env.push(NUM(Real.e))
    /*
    @s -> (x NUM)
    Φ (Golden Ratio).
     */
    case "$PHI" => env.push(NUM(Real.phi))
    /*
    @s -> (x NUM)
    Uniformly random number.
     */
    case "$rng" => env.push(NUM(random))
    /*
    @s -> (x NUM)
    Current line number of program execution.
     */
    case "$L" => getLNum
    /*
    @s -> (x STR)
    Current file of program execution.
     */
    case "$F" => getLFile
    /*
    @s -> (x SEQ[(y NUM)*])
    Infinite list of 0 to ∞.
     */
    case "$W" => env.push(LazyList.from(0).map(NUM(_)).toSEQ)
    /*
    @s -> (x SEQ[(y NUM)*])
    Infinite list of 1 to ∞.
     */
    case "$N" => env.push(LazyList.from(1).map(NUM(_)).toSEQ)
    /*
    @s -> (x SEQ[(y NUM)*])
    Infinite list of primes.
     */
    case "$P" => env.push(prime.lazyList.map(NUM(_)).toSEQ)
    /*
    @s -> (x STR) | UN
    Current line.
     */
    case "g@" => getLHere
    /*
    @s -> (x STR) | UN
    Next line.
     */
    case "g;" => getLNext
    /*
    @s -> (x STR) | UN
    Previous line.
     */
    case "g;;" => getLPrev

    /*
    @s -> (x STR)
    Line from STDIN.
     */
    case "i>" => in
    /*
    @s (a >STR) ->
    Sends `a` to STDOUT.
     */
    case ">o" => out
    /*
    @s (a >STR) ->
    #{>o}s `a` with trailing newline.
     */
    case "n>o" => outn
    /*
    @s a ->
    #{form}s and #{n>o}s `a`.
     */
    case "f>o" => outf

    /*
    @s a -> a a
     */
    case "dup" => dup
    /*
    @s a* -> a* (x ARR[a*])
     */
    case "dups" => dups
    /*
    @s a b -> a b a
     */
    case "over" => over
    /*
    @s (a @ n) b* (n >NUM) -> a b* a
    #{dup}s `n`th item from top of stack.
     */
    case "pick" => pick
    /*
    @s _ ->
     */
    case "pop" => pop
    /*
    @s _* ->
     */
    case "clr" => clr
    /*
    @s _ b -> b
     */
    case "nip" => nip
    /*
    @s (a @ n) b* (n >NUM) -> b*
    #{pop}s `n`th item from top of stack.
     */
    case "nix" => nix
    /*
    @s a b -> b a
     */
    case "swap" => swap
    /*
    @s a* -> x*
    Reverses stack.
     */
    case "rev" => rev
    /*
    @s a b -> b a b
     */
    case "tuck" => tuck
    /*
    @s (a @ n) b* c (n >NUM) -> c b* a
    #{swap}s `c` with `n`th item from top of stack.
     */
    case "trade" => trade
    /*
    @s a b c -> b c a
     */
    case "rot" => rot
    /*
    @s a b c -> c a b
     */
    case "rot_" => rotu
    /*
    @s (a @ n) b* (n >NUM) -> b* a
    #{rot}s to top `n`th item from top of stack.
     */
    case "roll" => roll
    /*
    @s b* c (n >NUM) -> (c @ n) b*
    #{rot_}s `c` to `n`th from top of stack.
     */
    case "roll_" => rollu
    /*
    @s a* b (f >FN) -> x* b
    #{pop}s `b`, executes `f`, and pushes `b`.
     */
    case "dip" => dip

    /*
    @s a -> (a FN)
    Wraps `a` in `FN`.
     */
    case "\\" => wrapFN
    /*
    @s a* f -> x*
    Executes `f`.
     */
    case "#" => eval
    /*
    @s f' -> x'
    Evaluates `f` (#{#} but only preserves resulting top of stack).
     */
    case "Q" => quar
    /*
    @s a* (n >NUM) -> x*
    #{#}s `n`th line.
     */
    case "@@" => evalLine
    /*
    @s a* (n >NUM) -> x*
    #{#}s `n`th line relative to current line.
     */
    case "@~" => evalLRel
    /*
    @s a* -> x*
    #{#}s current line.
     */
    case "@" => evalLHere
    /*
    @s a* -> x*
    #{#}s next line.
     */
    case ";" => evalLNext
    /*
    @s a* -> x*
    #{#}s previous line.
     */
    case ";;" => evalLPrev
    /*
    @s (n >NUM) -> (x STR) | UN
    `n`th line.
     */
    case "g@@" => getLn
    /*
    @s (n >NUM) -> (x STR) | UN
    `n`th line relative to current line.
     */
    case "g@~" => getLRel
    /*
    @s a* b f -> x*
    #{#}s `f` if `b` is truthy.
     */
    case "&#" => evalAnd
    /*
    @s a* b f -> x*
    #{#}s `f` if `b` is falsy.
     */
    case "|#" => evalOr
    /*
    @s a* b f g -> x*
    #{#}s `f` if `b` is truthy; else #{#}s `g`.
     */
    case "?#" => evalIf
    /*
    @s a* f (n >NUM) -> x*
    #{#}s `f` `n` times.
     */
    case "*#" => evalTimes
    /*
    @s a* f g -> x*
    Tries to #{#} `f`; on error, pushes caught `ERR` and #{#}s `g`.
     */
    case "!#" => evalTry
    /*
    @s (e ERR) ->
    Throws `e`.
     */
    case ">!" => throwERR
    /*
    @s (a >ARR) f -> (x ARR)
    #{#}s `f` on `a` as if it were a stack.
     */
    case "'" => evalArrSt
    /*
    @s (a* >ARR) f -> x*
    #{#}s `f` on the stack as if it were an `ARR`.
     */
    case "'_" => evalStArr

    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    `a * 10 ^ b`
     */
    case "E" => scale
    /*
    @s (a >NUM)' -> (x NUM)'
    Rounds `a` towards 0.
     */
    case "I" => trunc
    /*
    @s (a >NUM)' -> (x NUM)'
    Rounds `a` towards -∞.
     */
    case "|_" => floor
    /*
    @s (a >NUM)' -> (x NUM)'
    Rounds `a` to nearest integer.
     */
    case "|~" => round
    /*
    @s (a >NUM)' -> (x NUM)'
    Rounds `a` towards ∞.
     */
    case "|^"  => ceil
    case "X>b" => ???
    case "b>X" => ???
    /*
    @s (a >NUM)' -> (x NUM)'
    `-a`
     */
    case "_" => neg
    /*
    @s (a >STR)' -> (x STR)'
    Atom-reverses `a`.
     */
    case "__" => neg$
    /*
    @s a -> x
    Reverses `a`.
     */
    case "_`" => neg$$
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    `a + b`
     */
    case "+" => add
    /*
    @s (a >STR)' (b >STR)' -> (x STR)'
    Atomic #{+`}.
     */
    case "++" => add$
    /*
    @s a b -> x
    Concatenates `a` and `b`.
     */
    case "+`" => add$$
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    `a - b`
     */
    case "-"  => sub
    case "--" => sub$
    case "-`" => sub$$
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    `a * b`
     */
    case "*"  => mul
    case "**" => mul$
    case "*`" => mul$$
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    `a / b`. Throws error if `b` is 0.
     */
    case "/" => div
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    Integer #{/}.
     */
    case "/~" => divi
    case "//" => div$
    case "/`" => div$$
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    `a (mod b)`
     */
    case "%" => mod
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)' (y NUM)'
    Results of #{/~} and #{%} on `a` and `b`.
     */
    case "/%" => divmod
    case "%%" => mod$
    case "%`" => mod$$
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    `a ^ b`. Throws error if result would be a complex number.
     */
    case "^" => pow
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    #{^} but `b` is coerced to `int`.
     */
    case "^~" => powi
    case "^^" => ???
    case "^`" => ???
    /*
    @s (a >NUM)' -> (x NUM)'
    `e ^ a`
     */
    case "e^" => exp
    /*
    @s (a >NUM)' -> (x NUM)'
    Absolute value of `a`.
     */
    case "abs" => abs

    /*
    @s (a >NUM)' -> (x NUM)'
    Sine of `a`.
     */
    case "sin" => sin
    /*
    @s (a >NUM)' -> (x NUM)'
    Cosine of `a`.
     */
    case "cos" => cos
    /*
    @s (a >NUM)' -> (x NUM)'
    Tangent of `a`.
     */
    case "tan" => tan
    /*
    @s (a >NUM)' -> (x NUM)'
    Arcsine of `a`.
     */
    case "sin_" => asin
    /*
    @s (a >NUM)' -> (x NUM)'
    Arccosine of `a`.
     */
    case "cos_" => acos
    /*
    @s (a >NUM)' -> (x NUM)'
    Arctangent of `a`.
     */
    case "tan_" => atan
    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    Arctangent of `a` with `b` as quadrant.
     */
    case "tan_II" => atan2
    /*
    @s (a >NUM)' -> (x NUM)'
    Hyperbolic sine of `a`.
     */
    case "sinh" => sinh
    /*
    @s (a >NUM)' -> (x NUM)'
    Hyperbolic cosine of `a`.
     */
    case "cosh" => cosh
    /*
    @s (a >NUM)' -> (x NUM)'
    Hyperbolic tangent of `a`.
     */
    case "tanh" => tanh
    /*
    @s (a >NUM)' -> (x NUM)'
    Hyperbolic arcsine of `a`.
     */
    case "sinh_" => asinh
    /*
    @s (a >NUM)' -> (x NUM)'
    Hyperbolic arccosine of `a`.
     */
    case "cosh_" => acosh
    /*
    @s (a >NUM)' -> (x NUM)'
    Hyperbolic arctangent of `a`.
     */
    case "tanh_" => atanh

    /*
    @s (a >NUM)' (b >NUM)' -> (x NUM)'
    Base `b` logarithm of `a`.
     */
    case "log" => log
    /*
    @s (a >NUM)' -> (x NUM)'
    Natural logarithm of `a`.
     */
    case "ln" => ln
    /*
    @s (a >NUM)' -> (x NUM)'
    Base-10 logarithm of `a`.
     */
    case "logX" => log10

    /*
    @s (a >NUM)' -> (x NUM)'
    Whether `a` is prime. Uses a strong pseudo-primality test with a 1/1e12 chance of being wrong.
     */
    case "P?" => isPrime
    /*
    @s (a >NUM)' -> (x MAP[((y NUM) => (z NUM))*])'
    Prime-factorizes `a` into pairs of prime `y` and frequency `z`.
     */
    case "P/" => factor

    /*
    @s a' -> (x NUM)'
    Atomic #{!`}.
     */
    case "!" => not
    /*
    @s a -> (x NUM)
    Logical NOT.
     */
    case "!`" => not$$
    /*
    @s a' b' -> (a | b)'
    Atomic #{&`}.
     */
    case "&" => min
    /*
    @s a' b' -> (x NUM)'
    Atomic #{&&`}.
     */
    case "&&" => and
    /*
    @s a b -> a | b
    Minimum of `a` and `b`.
     */
    case "&`" => min$$
    /*
    @s a b -> (x NUM)
    Logical AND of `a` and `b`.
     */
    case "&&`" => and$$
    /*
    @s a' b' -> (a | b)'
    Atomic #{|`}.
     */
    case "|" => max
    /*
    @s a' b' -> (x NUM)'
    Atomic #{||`}.
     */
    case "||" => or
    /*
    @s a b -> a | b
    Maximum of `a` and `b`.
     */
    case "|`" => max$$
    /*
    @s a b -> (x NUM)
    Logical OR of `a` and `b`.
     */
    case "||`" => or$$
    /*
    @s a' b' -> (x NUM)'
    Atomic #{<=>`}.
     */
    case "<=>" => cmp
    /*
    @s a b -> x
    Comparison (-1, 0, or 1 depending on whether `a` is less than, equal to, or greater than `b`).
     */
    case "<=>`" => cmp$$
    /*
    @s a' b' -> (x NUM)'
    Atomic #{=`}.
     */
    case "=" => eql
    /*
    @s a b -> (x NUM)
    Whether `a` equals `b`.
     */
    case "=`" => eql$$
    /*
    @s a' b' -> (x NUM)'
    Atomic #{!=`}.
     */
    case "!=" => neq
    /*
    @s a b -> (x NUM)
    Whether `a` does not equals `b`.
     */
    case "!=`" => neq$$
    /*
    @s a' b' -> (x NUM)'
    Atomic #{<`}.
     */
    case "<" => lt
    /*
    @s a b -> (x NUM)
    Whether `a` is less than `b`.
     */
    case "<`" => lt$$
    /*
    @s a' b' -> (x NUM)'
    Atomic #{>`}.
     */
    case ">" => gt
    /*
    @s a b -> (x NUM)
    Whether `a` is greater than `b`.
     */
    case ">`" => gt$$
    /*
    @s a' b' -> (x NUM)'
    Atomic #{<=`}.
     */
    case "<=" => lteq
    /*
    @s a b -> (x NUM)
    Whether `a` is less than or equal to `b`.
     */
    case "<=`" => gt$$
    /*
    @s a' b' -> (x NUM)'
    Atomic #{>=`}.
     */
    case ">=" => gteq
    /*
    @s a b -> (x NUM)
    Whether `a` is greater than or equal to `b`.
     */
    case ">=`" => gt$$

    /*
    @s a i' -> (a.x | UN)'
    Value at atomic index `i` in `a`.
     */
    case ":" => get
    /*
    @s a -> a.x
    Value at random index in `a`.
     */
    case ":r" => getr
    /*
    @s a i -> a.x | UN
    Value at index `i` in `a`.
     */
    case ":`" => get$$
    /*
    @s a b' -> (x NUM)'
    Whether `a` has atomic `b`.
     */
    case ":?" => has
    /*
    @s a b -> (x NUM)
    Whether `a` has `b`. `MAP`s check `b` against keys; other types of `a` check `b` against values.
     */
    case ":?`" => has$$
    /*
    @s a -> (x NUM)
    Length of `a`.
     */
    case "len" => len
    /*
    @s a b -> (x ARR[a b])
    Pairs `a` and `b` in an `ARR`.
     */
    case "," => wrap$
    /*
    @s a -> (x ARR[a])
    Wraps `a` in an `ARR`.
     */
    case ",," => wrap
    /*
    @s a* -> a
    Wraps stack in an `ARR`.
     */
    case ",`" => wrap$$
    /*
    @s a -> a*
    Unwraps `a`.
     */
    case ",_" => unwrap
    /*
    @s _* a -> a*
    Replaces stack with `a` unwrapped.
     */
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
    case "a>b"   => range
    case "O>a"   => env.push(NUM(0)).swap.range
    case "a>O"   => env.push(NUM(0)).range
    case "I>a"   => env.push(NUM(1)).swap.range
    case "a>I"   => env.push(NUM(1)).range
    case "shuf"  => shuffle
    case "perm"  => perm
    case "comb"  => comb
    case "^set"  => powset

    case "S>c" => ???
    case "c>S" => ???
    case "<>"  => split
    case "<>:" => ???
    case "c<>" => env.push(STR("")).split
    case "w<>" => env.push(STR(" ")).split
    case "n<>" => env.push(STR("\n")).split
    case "s<>" => ssplit
    case "<>`" => ???
    case "><"  => join
    case "c><" => env.push(STR("")).join
    case "w><" => env.push(STR(" ")).join
    case "n><" => env.push(STR("\n")).join
    case "><`" => ???
    case "A>a" => ???
    case "a>A" => ???

    case "map"   => map
    case "tap"   => tapMap
    case "zip"   => zip
    case "tbl"   => tbl
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
