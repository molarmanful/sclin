package sclin

import scala.collection.immutable.VectorMap
import scala.util.chaining._
import ANY._

class LambdaSuite extends TU:

  "empty" |? "()" ==> dFN(0)
  "singleton" |? "\\asdf" ==> dFN(0, CMD("asdf"))
  "nested" |? "(()(()))" ==> dFN(0, "()(())".map(_.toString.pipe(CMD(_)))*)
  "open end" |? "(()((" =*> Vector(dFN(0, CMD("("), CMD(")"), CMD("(")), dFN(0))
