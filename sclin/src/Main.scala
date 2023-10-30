package sclin

import better.files.*
import mainargs.*
import scala.util.chaining.*

object Main:

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args.toIndexedSeq, allowPositional = true)

  @main def sclin(
      @arg(short = 'f', doc = "Execute file.") file: Option[File],
      @arg(short = 'e', doc = "Execute string.") eval: Option[String],
      @arg(hidden = true) doceval: Option[String],
      @arg(short = 's', doc = "Step mode.") step: Flag,
      @arg(short = 'v', doc = "Verbose mode.") verb: Flag,
      @arg(short = 'i', doc = "Implicit mode.") impl: Flag,
      @arg(doc = "Enable/disable ANSI in debug messages.") nocolor: Flag
  ): Unit =
    def err(e: String): Unit = println(cflag(fansi.Color.Red)(e))
    def cflag(x: fansi.Attrs) =
      if nocolor.value then fansi.Attrs() else x
    try
      doceval match
        case Some(s) => ENV.docRun(s)
        case _ =>
          val flags = Flags(
            s = step.value,
            v = verb.value,
            i = impl.value,
            nc = nocolor.value
          )
          file match
            case Some(f) => ENV.run(f.contentAsString, file, flags, cflag)
            case _ =>
              eval match
                case Some(s) => ENV.run(s, file, flags, cflag)
                case _       => ()
    catch
      case e: java.nio.file.NoSuchFileException =>
        err(s"no file ${e.getFile}")
      case e: LinERR => err(e.toString)
      case e =>
        err:
          s"ERR: $e\n     ---\n${e.getStackTrace.map("     " + _).mkString("\n")}"

  given TokensReader.Simple[File] with

    def shortName                                     = "path"
    def read(strs: Seq[String]): Right[Nothing, File] = Right(File(strs.head))
