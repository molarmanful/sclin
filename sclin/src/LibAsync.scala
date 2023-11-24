package sclin

import cats.effect.ExitCase
import monix.eval.Task
import monix.reactive.Observable
import monix.reactive.OverflowStrategy
import scala.collection.immutable.VectorMap
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.util.chaining.*
import ANY.*

extension (env: ENV)

  def evalTASK: ENV = env.arg1: (x, env) =>
    env.push(x.vec1(f => Task(env.push(f).quar.getStack(0)).toTASK))
  def await: ENV =
    env.vec1(x => Await.result(x.toFUT.x, Duration.Inf))
  def cancelFUT: ENV = env.arg1: (x, env) =>
    x.vec1(_.toFUT.x.cancel().pipe(_ => UN))
    env
  def asyncBound: ENV = env.mod1:
    case x: OBS => x.modOBS(_.asyncBoundary(OverflowStrategy.Unbounded))
    case x      => x.vec1(_.modTASK(_.asyncBoundary))
  def forkTASK: ENV   = env.vec1(_.modTASK(_.executeAsync))
  def forkIOTASK: ENV = env.vec1(_.modTASK(_.executeOn(env.ioSched)))
  def memoTASK: ENV = env.mod1:
    case OBS(x) => x.cache.toOBS
    case x      => x.vec1(_.modTASK(_.memoize))
  def memoTASK$ : ENV = env.vec1(_.modTASK(_.memoizeOnSuccess))
  def uncancelTASK: ENV = env.mod1:
    case OBS(x) => x.uncancelable.toOBS
    case x      => x.vec1(_.modTASK(_.uncancelable))
  def timeoutTASK: ENV = env.vec2: (x, n) =>
    val n1 = n.toNUM.x.toLong
    x.modTASK:
      _.timeoutWith(n1.milliseconds, LinEx("TASK", s"timeout after ${n1}ms"))
  def itrTASKW(t: ANY, f: Iterable[Task[ANY]] => Task[Iterable[ANY]]): ANY =
    t match
      case MAP(x) =>
        x.values
          .map(_.toTASK.x)
          .pipe(f)
          .map:
            _.zip(x.keys)
              .map:
                case (v, k) => (k, v)
              .to(VectorMap)
              .toMAP
          .toTASK
      case Itr(x) =>
        x.toSEQ.x
          .map(_.toTASK.x)
          .pipe(f)
          .map(_.mSEQ(x))
          .toTASK
      case x => x.toTASK
  def seqTASK: ENV = env.mod1(itrTASKW(_, Task.sequence))
  def parTASK: ENV = env.mod1(itrTASKW(_, Task.parSequence))
  def parnTASK: ENV =
    env.mod2((x, n) => itrTASKW(x, Task.parSequenceN(n.toInt)))
  def parunTASK: ENV = env.mod1(itrTASKW(_, Task.parSequenceUnordered))
  def raceTASK: ENV =
    env.mod1(_.toSEQ.x.map(_.toTASK.x).pipe(Task.raceMany).toTASK)
  def timeTASK: ENV = env.vec1:
    _.modTASK:
      _.timed.map:
        case (t, a) => Vector(t.toMillis.toNUM, a).toARR
  def redeemTASK: ENV = env.vec3: (x, f, g) =>
    x.modTASK:
      _.redeemWith(
        e => env.SIG_1f1(f)(ERR(e)).toTASK.x,
        env.SIG_1f1(g)(_).toTASK.x
      )
  def restartwTASK: ENV = env.mod2: (x, y) =>
    x match
      case _: OBS => y.vec1(f => x.modTASK(_.restartUntil(env.SIG_1fb(f))))
      case _      => x.vec2(y)((t, f) => t.modTASK(_.restartUntil(env.SIG_1fb(f))))
  def wrapTRYTASK: ENV   = env.vec1(_.modTASK(_.materialize.map(_.toTRY)))
  def unwrapTRYTASK: ENV = env.vec1(_.modTASK(_.map(_.toTRY.x).dematerialize))
  def bracketTASK: ENV = env.mod3: (x, y, z) =>
    def go(f: ANY)(a: ANY, e: ExitCase[Throwable]) =
      env.SIG_2f1(f)(a, ANY.exitCase(e)).toTASK.x.map(_ => ())
    x match
      case _: OBS =>
        y.vec2(z): (f, g) =>
          x.modOBS(_.bracketCase(env.SIG_1f1(f)(_).toOBS.x)(go(g)))
      case _ =>
        x.vec3(y, z): (t, f, g) =>
          t.modTASK(_.bracketCase(env.SIG_1f1(f)(_).toTASK.x)(go(g)))
  def onErrTASK: ENV = env.mod2: (x, y) =>
    x match
      case _: OBS =>
        y.vec1: f =>
          x.modOBS(_.onErrorHandleWith(e => env.SIG_1f1(f)(ERR(e)).toOBS.x))
      case _ =>
        x.vec2(y): (t, f) =>
          t.modTASK(_.onErrorHandleWith(e => env.SIG_1f1(f)(ERR(e)).toTASK.x))
  def sleepTASK: ENV = env.vec1: n =>
    val n1 = n.toNUM.x.toLong
    n1.milliseconds.pipe(Task.sleep).map(_ => NUM(n1)).toTASK
  def delayTASK: ENV = env.mod2: (x, y) =>
    x match
      case _: OBS => y.vec1(n => x.modOBS(_.delayExecution(n.toMs)))
      case _      => x.vec2(y)((t, n) => t.modTASK(_.delayExecution(n.toMs)))

  def ostratUn: ENV = env.push(OSTRAT(OverflowStrategy.Unbounded))
  def ostratFail: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.Fail.apply).pipe(OSTRAT(_)))
  def ostratBack: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.BackPressure.apply).pipe(OSTRAT(_)))
  def ostratNew: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.DropNew.apply).pipe(OSTRAT(_)))
  def ostratOld: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.DropOld.apply).pipe(OSTRAT(_)))
  def ostratClr: ENV =
    env.vec1(_.toInt.pipe(OverflowStrategy.ClearBuffer.apply).pipe(OSTRAT(_)))

  def ocache: ENV = env.mod2((x, y) => y.vec1(n => x.modOBS(_.cache(n.toInt))))
  def obufferN: ENV = env.mod2: (x, y) =>
    y.vec1(n => x.modOBS(_.bufferIntrospective(n.toInt).map(_.toARR)))
  def obufferT: ENV = env.mod2: (x, y) =>
    y.vec1(n => x.modOBS(_.bufferTimed(n.toMs).map(_.toARR)))
  def obufferTN: ENV = env.mod3: (x, y, z) =>
    y.vec2(z): (t, n) =>
      x.modOBS(_.bufferTimedAndCounted(t.toMs, n.toInt).map(_.toARR))
  def obufferTB: ENV = env.modx(4):
    case Vector(x, y, v, w) =>
      y.vec3(v, w): (t, n, f) =>
        x.modOBS:
          _.bufferTimedWithPressure(t.toMs, n.toInt, env.SIG_1f1(f)(_).toInt)
            .map(_.toARR)
  def obufferON: ENV = env.mod3: (x, y, z) =>
    z.vec1(n => x.modOBS(_.bufferWithSelector(y.toOBS.x, n.toInt).map(_.toARR)))
  def othrottle: ENV = env.mod3: (x, y, z) =>
    y.vec2(z)((t, n) => x.modOBS(_.throttle(t.toMs, n.toInt).map(_.toARR)))
  def othrottleFirst: ENV =
    env.mod2((x, y) => y.vec1(n => x.modOBS(_.throttleFirst(n.toMs))))
  def othrottleLast: ENV =
    env.mod2((x, y) => y.vec1(n => x.modOBS(_.throttleLast(n.toMs))))
  def odebounce: ENV =
    env.mod2((x, y) => y.vec1(n => x.modOBS(_.debounce(n.toMs))))
  def oasyncBound: ENV =
    env.mod2((x, y) => x.modOBS(_.asyncBoundary(y.toOSTRAT.x)))
  def odelay: ENV =
    env.mod2((x, y) => x.modOBS(_.delayExecutionWith(y.toOBS.x)))
  def ointervald: ENV =
    env.vec1(n => Observable.interval(n.toMs).map(NUM(_)).toOBS)
  def ointervalr: ENV =
    env.vec1(n => Observable.intervalAtFixedRate(n.toMs).map(NUM(_)).toOBS)
