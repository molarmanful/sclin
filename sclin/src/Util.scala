package sclin

import better.files.*
import java.nio.charset.Charset
import spire.implicits.*
import spire.math.*

object Util:

  def asinh(x: Double): Double = log(x + (x * x + 1).sqrt)
  def acosh(x: Double): Double = log(x + (x * x - 1).sqrt)
  def atanh(x: Double): Double = asinh(x).fquot(acosh(x))

  def fromDec(n: SafeLong, b: Int): ARRW[SafeLong] =
    def loop(
        n: SafeLong,
        b: Int = b,
        res: ARRW[SafeLong] = Vector()
    ): ARRW[SafeLong] =
      if b == 0 then Vector()
      else if b == 1 then Vector.fill(n.toInt)(1)
      else if b < 0 then loop(n, -b).reverse
      else if n == 0 then
        res match
          case Vector() if b > 1 => Vector(0)
          case x                 => x
      else loop(n / b, b, n % b +: res)
    loop(n)

  def toDec(ns: ARRW[SafeLong], b: SafeLong): SafeLong =
    if b == 0 then 0
    else if b < 0 then toDec(ns.reverse, -b)
    else if b == 1 then ns.length
    else
      ns.reverseIterator.zipWithIndex.foldLeft[SafeLong](0):
        case (a, (n, i)) =>
          b ** i * n + a

  def cProd[A](ls: SEQW[SEQW[A]]): SEQW[SEQW[A]] = ls match
    case LazyList()       => LazyList()
    case x #:: LazyList() => x.map(LazyList(_))
    case x #:: xs =>
      val y = cProd(xs)
      x.flatMap(a => y.map(b => a #:: b))

  def cPow[A](seed: SEQW[A], n: Int): SEQW[SEQW[A]] = cProd:
    LazyList.fill(n)(seed)

  def transpose[A](ls: SEQW[SEQW[A]]): SEQW[SEQW[A]] =
    ls.filter(_.nonEmpty) match
      case LazyList() => LazyList()
      case xs         => xs.map(_.head) #:: transpose(xs.map(_.tail))

  def charset(s: String): Charset = s match
    case "" => Charset.defaultCharset
    case _  => s

  def abtobs(s: Array[Byte]): String = s.map(_.&(0xff).toChar).mkString
  def bstoab(s: String): Array[Byte] = s.toString.getBytes("ISO-8859-1")
