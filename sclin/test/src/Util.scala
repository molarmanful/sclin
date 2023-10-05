package sclin

import ANY._

trait TU extends munit.FunSuite:

  def dP(l: Int): PATH     = PATH(None, l)
  def dFN(l: Int, x: ANY*) = FN(dP(l), LazyList(x*))

  extension (s: String)

    def |?(x: => Boolean): Unit    = test(s)(assert(x))
    def ==>(x: ANY): Boolean       = ENV.run(s).getStack(0) == x
    def =*>(x: ARRW[ANY]): Boolean = ENV.run(s).stack == x
