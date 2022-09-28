import mill._
import scalalib._

object main extends ScalaModule {

  def scalaVersion  = "3.2.0"
  def scalacOptions = Seq("-deprecation", "-feature")
  def ivyDeps = Agg(
    ivy"org.apfloat:apfloat:1.10.1",
    ivy"com.lihaoyi::mainargs:0.2.3",
    ivy"com.lihaoyi::os-lib:0.8.0",
    ivy"com.lihaoyi::fansi:0.4.0"
  )

  def cmdoc = T {
    val src = os.read
      .lines(os.pwd / "main" / "src" / "Lib.scala")
      .dropWhile(_.trim != "// CMDOC START")
      .takeWhile(_.trim != "// CMDOC END")
      .tail
    println(src)
  }

  object test extends Tests with TestModule.Munit {

    def ivyDeps = Agg(
      ivy"org.scalameta::munit:1.0.0-M6"
    )

  }

}
