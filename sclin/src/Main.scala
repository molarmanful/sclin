package sclin

import mainargs._
import scala.util.chaining._

object Main:

  implicit object PathRead
      extends TokensReader[os.Path](
        "path",
        strs => Right(os.Path(strs.head, os.pwd))
      )

  @main
  def run(
      @arg(short = 'f', doc = "Execute file.") file: Option[os.Path],
      @arg(short = 'e', doc = "Execute string.") eval: Option[String],
      @arg(
        doc = "(Internal) Execute string with doc-style result."
      ) doceval: Option[String],
      @arg(short = 's', doc = "Step mode.") step: Flag,
      @arg(short = 'v', doc = "Verbose mode.") verb: Flag,
      @arg(short = 'i', doc = "Implicit mode.") impl: Flag
  ) = doceval match
    case Some(s) => ENV.docRun(s)
    case _ =>
      def err(e: String) = println(fansi.Color.Red(e))
      val flags          = (step.value, verb.value, impl.value)
      try
        file match
          case Some(f) => ENV.run(os.read(f), file, flags)
          case _ =>
            eval match
              case Some(s) => ENV.run(s, file, flags)
              case _       => ()
      catch
        case e: java.nio.file.NoSuchFileException =>
          err(s"no file ${e.getFile()}")
        case e: LinERR => err(e.toString)
        case e         => err(s"ERR: $e").tap(_ => e.printStackTrace)

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args.toIndexedSeq, allowPositional = true)
