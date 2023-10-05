package sclin

import ANY._

trait TU extends munit.FunSuite:

  def dP(l: Int): PATH     = PATH(None, l)
  def dFN(l: Int, x: ANY*) = FN(dP(l), LazyList(x*))
  def dARR(x: ANY*)        = ARR(Vector(x*))
  def dSEQ(x: ANY*)        = SEQ(LazyList(x*))

  extension (s: String)

    def |?(x: => (Any, Any)): Unit = x match
      case (a, b) => test(s)(assertEquals(a, b))
    def |??(x: => Boolean): Unit  = test(s)(assert(x))
    def ==>(x: ANY)               = (ENV.run(s).getStack(0), x)
    def =+>(x: ARRW[ANY])         = (ENV.run(s).stack, x)
    def =*>(f: ENV => (Any, Any)) = f(ENV.run(s))
