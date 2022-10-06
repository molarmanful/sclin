import $file.docp
import mill._
import scala.util.chaining._
import scalalib._
// import scalanativelib._
// import scalanativelib.api._

object main extends ScalaModule {

  // def scalaNativeVersion = "0.4.7"
  def scalaVersion  = "3.2.0"
  def scalacOptions = Seq("-deprecation", "-feature")
  // def releaseMode        = ReleaseMode.ReleaseFast
  // def nativeLTO          = LTO.Thin
  def ivyDeps = Agg(
    ivy"org.typelevel::spire:0.18.0",
    ivy"com.lihaoyi::mainargs:0.2.3",
    ivy"com.lihaoyi::os-lib:0.8.0",
    ivy"com.lihaoyi::fansi:0.4.0"
  )

  def cmdoc = T {
    os.read
      .lines(os.pwd / "main" / "src" / "Lib.scala")
      .dropWhile(_.trim != "// CMDOC START")
      .takeWhile(_.trim != "// CMDOC END")
      .tail
      .pipe(docp.DocParser.parse)
      .pipe(_.md)
      .pipe(os.write.over(os.pwd / "sclin-docs" / "commands.md", _))
  }

  object test extends Tests with TestModule.Munit {

    def ivyDeps = Agg(
      ivy"org.scalameta::munit:1.0.0-M6"
    )

  }

}
