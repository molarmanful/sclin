import $file.docp
import $ivy.`com.goyeau::mill-scalafix::0.4.0`
import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import com.goyeau.mill.scalafix.ScalafixModule
import de.tobiasroeser.mill.vcs.version.VcsVersion
import mill._
import mill.contrib.scoverage.ScoverageModule
import scala.util.chaining._
import scalalib._
import scalalib.publish._
import scalalib.scalafmt._

object sclin
    extends ScoverageModule
    with PublishModule
    with ScalafmtModule
    with ScalafixModule {

  def scalaVersion              = "3.4.2"
  def scoverageVersion          = "2.0.11"
  def publishVersion: T[String] = VcsVersion.vcsState().format()
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
  def scalacOptions = Seq("-deprecation", "-feature", "-Wunused:all")
  def ivyDeps = Agg(
    ivy"org.typelevel::spire:0.18.0",
    ivy"com.lihaoyi::mainargs:0.7.4",
    ivy"com.lihaoyi::fansi:0.5.0",
    ivy"com.lihaoyi::upickle:4.0.1",
    ivy"com.lihaoyi::os-lib:0.10.4",
    ivy"io.monix::monix:3.4.1",
    ivy"io.monix::monix-nio:0.1.0",
    ivy"com.github.pathikrit::better-files:3.9.2"
  )

  def cmdoc() = T.command {
    os.read
      .lines(millSourcePath / "src" / "Cmd.scala")
      .dropWhile(_.trim != "// CMDOC START")
      .takeWhile(_.trim != "// CMDOC END")
      .tail
      .pipe(docp.DocParser.parse)
      .pipe(_.md)
      .tap(_ => os.makeDir.all(os.pwd / "sclin-docs-gen"))
      .pipe(os.write.over(os.pwd / "sclin-docs-gen" / "Commands.md", _))
  }

  object test extends ScoverageTests with TestModule.Munit with ScalafmtModule {

    def ivyDeps = Agg(ivy"org.scalameta::munit::1.0.1")

  }

}
