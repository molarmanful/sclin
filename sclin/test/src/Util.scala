package sclin

import scala.collection.immutable.HashMap
import scala.collection.immutable.VectorMap
import ANY.*

trait TU extends munit.FunSuite:

  def dP(l: Int): PATH     = PATH(None, l)
  def dFN(l: Int, x: ANY*) = FN(dP(l), HashMap(), LazyList(x*))
  def dARR(x: ANY*)        = ARR(Vector(x*))
  def dSEQ(x: ANY*)        = SEQ(LazyList(x*))
  def dMAP(x: (ANY, ANY)*) = MAP(VectorMap(x*))

  extension (s: String)

    def |?(x: => (Any, Any)): Unit = x match
      case (a, b) => test(s)(assertEquals(a, b))
    def |??(x: => Boolean): Unit  = test(s)(assert(x))
    def |?|(x: String): Unit      = test(s)(assert(ENV.run(x).getStack(0).toBool))
    def ==>(x: ANY)               = (ENV.run(s).getStack(0), x)
    def =+>(x: ARRW[ANY])         = (ENV.run(s).stack, x)
    def =*>(f: ENV => (Any, Any)) = f(ENV.run(s))
