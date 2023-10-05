package sclin

import ANY._

trait TU extends munit.FunSuite:

  def assertR(x: String, y: ANY)        = assert(ENV.run(x).getStack(0) == y)
  def assertRS(x: String, y: ARRW[ANY]) = assert(ENV.run(x).stack == y)

  def dP(l: Int): PATH     = PATH(None, l)
  def dFN(l: Int, x: ANY*) = FN(dP(l), LazyList(x*))

  extension (s: String) def |?(x: => Any): Unit = test(s)(x)
