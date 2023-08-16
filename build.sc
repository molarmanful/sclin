import $file.docp
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import mill._
import scala.util.chaining._
import scalalib._
import scalalib.publish._

object sclin extends ScalaModule with PublishModule {

  def scalaVersion                       = "3.3.0"
  override def publishVersion: T[String] = VcsVersion.vcsState().format()
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
    ivy"com.lihaoyi::mainargs:0.5.1",
    ivy"com.lihaoyi::os-lib:0.9.1",
    ivy"com.lihaoyi::fansi:0.4.0",
    ivy"com.lihaoyi::upickle:3.1.2",
    ivy"io.monix::monix:3.4.1"
  )

  def cmdoc() = T.command {
    os.read
      .lines(millSourcePath / "src" / "Lib.scala")
      .dropWhile(_.trim != "// CMDOC START")
      .takeWhile(_.trim != "// CMDOC END")
      .tail
      .pipe(docp.DocParser.parse)
      .pipe(_.md)
      .pipe(os.write.over(os.pwd / "sclin-docs" / "commands.md", _))
  }

  object test extends ScalaTests with TestModule.Munit {

    def ivyDeps = Agg(ivy"org.scalameta::munit:1.0.0-M8")

  }

}
