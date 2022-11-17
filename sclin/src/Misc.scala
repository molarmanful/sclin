package sclin

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.VectorMap
import spire.math.Real
import util.chaining._

type FILE = Option[os.Path]

type ARRW[T]    = Vector[T]
type SEQW[T]    = LazyList[T]
type MAPW[K, V] = VectorMap[K, V]

type NUMF = Real

case class PATH(f: FILE, l: Int):

  override def toString: String = s"${f.getOrElse("?")}:$l"

case class LinEx(t: String, x: String) extends Exception(x):

  def toLinERR(env: ENV): LinERR = LinERR(env.code.p, t, x)

case class LinERR(p: PATH, t: String, x: String) extends Exception(x):

  override def toString: String = s"ERR($t): $x @ ($p)"

  override def equals(x: Any): Boolean = x match
    case x: LinERR => canEqual(x) && t == x.t

  override def hashCode: Int = t.##
