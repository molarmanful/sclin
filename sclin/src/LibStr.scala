package sclin

import scala.util.chaining.*
import spire.math.*
import ANY.*

extension (env: ENV)

  def padH(f: (String, Int, String) => String): ENV = env.vec3: (x, y, z) =>
    val x1 = x.toString
    val y1 = y.toInt - x1.length
    val z1 = z.toString
    f(x1, y1, z1).sSTR
  def padH1(s: String, n: Int): String =
    LazyList.continually(s).flatten.take(n).mkString
  def pad: ENV  = padH((x, y, z) => x + padH1(z, y))
  def padl: ENV = padH((x, y, z) => padH1(z, y) + x)
  def padc: ENV = padH: (x, y, z) =>
    val q  = y / 2
    val y1 = y - q
    padH1(z, q) + x + padH1(z, y1)

  def pad$H(f: (ANY, Int, ANY) => ANY): ENV = env.mod3: (x, y, z) =>
    y.vec1: y =>
      val y1 = y.toInt - x.length
      if x.toSEQ.x.drop(y1).isEmpty then f(x, y1, z) else x
  def pad$H1(s: ANY, n: Int): ANY = ARR(Vector()).add$$(s).mul$$(NUM(n))
  def pad$ : ENV                  = pad$H((x, y, z) => x.add$$(pad$H1(z, y)))
  def padl$ : ENV                 = pad$H((x, y, z) => pad$H1(z, y).add$$(x))
  def padc$ : ENV = pad$H: (x, y, z) =>
    val q  = y / 2
    val y1 = y - q
    pad$H1(z, q).add$$(x).add$$(pad$H1(z, y1))

  def toCodePt: ENV =
    env.mod1(_.vec1(x => x.toString.map(_.toInt.pipe(NUM(_))).toARR))
  def fromCodePt: ENV = env.mod1:
    _.map(_.toInt.toChar.toString.sSTR).toString.sSTR

  def split: ENV   = env.str2a(_.split(_))
  def ssplit: ENV  = env.str1a(_.split(raw"\s"))
  def join: ENV    = env.mod2((x, y) => y.str1(x.join))
  def toUpper: ENV = env.str1(_.toUpperCase)
  def toLower: ENV = env.str1(_.toLowerCase)
  def toCap: ENV   = env.str1(_.capitalize)

  def rmatchBase(x: ANY, r: ANY): Iterator[MAP] =
    r.toString.r.findAllMatchIn(x.toString).map(_.matchMAP)
  def rmatch: ENV       = env.vec2(rmatchBase(_, _).toSEQ)
  def rmatchMatch: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("&"))).toSEQ)
  def rmatchBefore: ENV = env.vec2(rmatchBase(_, _).map(_.get(STR("`"))).toSEQ)
  def rmatchAfter: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("'"))).toSEQ)
  def rmatchGroups: ENV = env.vec2(rmatchBase(_, _).map(_.get(STR("*"))).toSEQ)
  def rmatchStart: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("^"))).toSEQ)
  def rmatchEnd: ENV    = env.vec2(rmatchBase(_, _).map(_.get(STR("$"))).toSEQ)
  def rsub: ENV = env.vec3: (x, r, f) =>
    r.toString.r
      .replaceAllIn(x.toString, m => env.evalA1(Vector(m.matchMAP), f).toString)
      .sSTR
  def rsubFirst: ENV =
    env.vec3((x, r, f) =>
      r.toString.r.replaceFirstIn(x.toString, f.toString).sSTR
    )
