package sclin

import monix.execution.CancelableFuture
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.HashMap
import scala.collection.immutable.VectorMap
import spire.implicits._
import spire.math._
import util.chaining._

type FILE = Option[os.Path]

type ARRW[T]    = Vector[T]
type SEQW[T]    = LazyList[T]
type MAPW[K, V] = VectorMap[K, V]
type FUTW[T]    = CancelableFuture[T]

type NUMF = Real

type CALLS  = SEQW[(PATH, ANY)]
type SCOPE  = HashMap[String, ANY]
type GSCOPE = TrieMap[String, ANY]
type IDS    = HashMap[String, PATH]
type GIDS   = TrieMap[String, PATH]

case class PATH(f: FILE, l: Int):

  override def toString: String = s"${f.getOrElse("?")}:$l"

case class LinEx(t: String, x: String) extends Exception(x):

  def toLinERR(env: ENV): LinERR =
    LinERR(env, t, x)

case class LinERR(env: ENV, t: String, x: String) extends Exception(x):

  override def toString: String =
    def f(p: PATH, c: ANY): String = s"\n    @ $p ${c.toForm}"
    s"ERR($t): $x${f.tupled(env.curPC)}${env.calls.map(f.tupled).mkString}"

  override def equals(x: Any): Boolean = x match
    case x: LinERR => canEqual(x) && t == x.t

  override def hashCode: Int = t.##

object Util:

  def asinh(x: Double): Double = log(x + (x * x + 1).sqrt)
  def acosh(x: Double): Double = log(x + (x * x - 1).sqrt)
  def atanh(x: Double): Double = asinh(x).fquot(acosh(x))
