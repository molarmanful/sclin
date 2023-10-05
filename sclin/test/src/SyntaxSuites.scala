package sclin

import scala.collection.immutable.VectorMap
import scala.util.chaining._
import ANY._

class LambdaSuite extends TU:

  "empty" |? assertR("()", dFN(0))
  "nested" |? assertR(
    "(()(()))",
    dFN(0, "()(())".map(_.toString.pipe(CMD(_)))*)
  )
  "singleton" |? assertR("\\asdf", dFN(0, CMD("asdf")))
