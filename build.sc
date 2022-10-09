import $file.docp
import mill._
import scalalib.publish._
import scala.util.chaining._
import scalalib._

object sclin extends ScalaModule with PublishModule {

  def scalaVersion   = "3.2.0"
  def publishVersion = "0.0.0-3"
  def pomSettings = PomSettings(
    description = "Scala implementation of lin",
    organization = "io.github.molarmanful",
    url = "https://github.com/molarmanful/sclin",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("molarmanful", "sclin"),
    developers = Seq(
      Developer("molarmanful", "Ben Pang", "https://github.com/molarmanful")
    )
  )
  def scalacOptions = Seq("-deprecation", "-feature")
  def ivyDeps = Agg(
    ivy"org.typelevel::spire:0.18.0",
    ivy"com.lihaoyi::mainargs:0.2.3",
    ivy"com.lihaoyi::os-lib:0.8.0",
    ivy"com.lihaoyi::fansi:0.4.0"
  )

  def cmdoc = T {
    os.read
      .lines(os.pwd / "sclin" / "src" / "Lib.scala")
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
