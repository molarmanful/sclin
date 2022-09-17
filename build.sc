import mill._
import scalalib._

object main extends ScalaModule {

  def scalaVersion  = "3.2.0"
  def scalacOptions = Seq("-deprecation")
  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:2.8.0",
    ivy"org.typelevel::spire:0.18.0",
    ivy"com.lihaoyi::mainargs:0.2.3",
    ivy"com.lihaoyi::os-lib:0.8.0",
    ivy"com.lihaoyi::fansi:0.4.0"
  )

  object test extends Tests with TestModule.Munit {

    def ivyDeps = Agg(
      ivy"org.scalameta::munit:1.0.0-M6"
    )

  }

}
