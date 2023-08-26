package sclin

import mainargs._
import scala.util.chaining._

object Main:

  implicit object PathRead extends TokensReader.Simple[os.Path]:

    def shortName               = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))

  @main def sclin(
      @arg(short = 'f', doc = "Execute file.")
      file: Option[os.Path],
      @arg(short = 'e', doc = "Execute string.")
      eval: Option[String],
      @arg(hidden = true)
      doceval: Option[String],
      @arg(short = 's', doc = "Step mode.")
      step: Flag,
      @arg(short = 'v', doc = "Verbose mode.")
      verb: Flag,
      @arg(short = 'i', doc = "Implicit mode.")
      impl: Flag,
      @arg(doc = "Enable/disable ANSI in debug messages.")
      nocolor: Flag
  ) = doceval match
    case Some(s) => ENV.docRun(s)
    case _ =>
      def cflag(x: fansi.Attrs) = if nocolor.value then fansi.Attrs.Empty else x
      def err(e: String)        = println(cflag(fansi.Color.Red)(e))
      val flags = Map(
        "s"  -> step.value,
        "v"  -> verb.value,
        "i"  -> impl.value,
        "nc" -> nocolor.value
      ).withDefaultValue(false)

      try
        file match
          case Some(f) =>
            ENV.run(os.read(f), file.map(_.toString), flags, cflag)
          case _ =>
            eval match
              case Some(s) => ENV.run(s, file.map(_.toString), flags, cflag)
              case _       => ()
      catch
        case e: java.nio.file.NoSuchFileException =>
          err(s"no file ${e.getFile()}")
        case e: LinERR => err(e.toString)
        case e         => err(s"ERR: $e").tap(_ => e.printStackTrace)

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args.toIndexedSeq, allowPositional = true)
