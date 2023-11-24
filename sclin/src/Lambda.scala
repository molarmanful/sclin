package sclin

import ANY.*

case class Lambda(
    xs: LazyList[ANY] = LazyList(),
    ys: LazyList[ANY] = LazyList(),
    n: Int = 1
):

  def loop: Lambda =
    if n <= 0 then this
    else
      xs match
        case LazyList() => this
        case c #:: cs =>
          val d = c match
            case CMD(x) =>
              val m = x.groupBy(c => c).view.mapValues(_.length)
              m.get('(').getOrElse(0) - m.get(')').getOrElse(0)
            case _ => 0
          Lambda(cs, ys #::: LazyList(c), n + d).loop
