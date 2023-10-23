import $file.docp
import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import mill._
import mill.contrib.scoverage.ScoverageModule
import scala.util.chaining._
import scalalib._
import scalalib.publish._

object sclin extends ScoverageModule with PublishModule {

  def scalaVersion                       = "3.3.1"
  def scoverageVersion                   = "2.0.11"
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
    ivy"com.lihaoyi::mainargs:0.5.4",
    ivy"com.lihaoyi::fansi:0.4.0",
    ivy"com.lihaoyi::upickle:3.1.3",
    ivy"io.monix::monix:3.4.1",
    ivy"io.monix::monix-nio:0.1.0",
    ivy"dev.zio::zio:2.0.18",
    ivy"dev.zio::zio-streams:2.0.18",
    ivy"dev.zio::zio-connect-file:0.4.4",
    ivy"com.lihaoyi::os-lib:0.9.1"
  )

  def cmdoc() = T.command {
    os.read
      .lines(millSourcePath / "src" / "Lib.scala")
      .dropWhile(_.trim != "// CMDOC START")
      .takeWhile(_.trim != "// CMDOC END")
      .tail
      .pipe(docp.DocParser.parse)
      .pipe(_.md)
      .tap(_ => os.makeDir.all(os.pwd / "sclin-docs-gen"))
      .pipe(os.write.over(os.pwd / "sclin-docs-gen" / "Commands.md", _))
  }

  object test extends ScoverageTests with TestModule.Munit {

    def ivyDeps = Agg(ivy"org.scalameta::munit::1.0.0-M10")

  }

}
