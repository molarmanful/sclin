package sclin

import mainargs._
import scala.util.chaining._
import zio._

object Main extends ZIOAppDefault:

  given TokensReader.Simple[os.Path] with

    def shortName               = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.head, os.pwd))

  @main case class Config(
      @arg(short = 'f', doc = "Execute file.") file: Option[os.Path],
      @arg(short = 'e', doc = "Execute string.") eval: Option[String],
      @arg(hidden = true) doceval: Option[String],
      @arg(short = 's', doc = "Step mode.") step: Flag,
      @arg(short = 'v', doc = "Verbose mode.") verb: Flag,
      @arg(short = 'i', doc = "Implicit mode.") impl: Flag,
      @arg(doc = "Enable/disable ANSI in debug messages.") nocolor: Flag
  ):

    def toFlags: Flags =
      Flags(s = step.value, v = verb.value, i = impl.value, nc = nocolor.value)

  def run =
    for
      args <- getArgs
      cfg <-
        ZIO.attempt(
          ParserForClass[Config].constructOrExit(args, allowPositional = true)
        )
      _ <-
        def cflag(x: fansi.Attrs) =
          if cfg.nocolor.value then fansi.Attrs() else x
        def err(e: String) = Console.printLine(cflag(fansi.Color.Red)(e))

        cfg.doceval match
          case Some(s) => ZIO.attemptBlocking(ENV.docRun(s))
          case _ =>
            val flags = cfg.toFlags
            try
              cfg.file match
                case Some(f) =>
                  ZIO.attempt(
                    ENV.run(os.read(f), cfg.file, flags, cflag)
                  )
                case _ =>
                  cfg.eval match
                    case Some(s) =>
                      ZIO.attempt(
                        ENV.run(s, cfg.file, flags, cflag)
                      )
                    case _ => ZIO.unit
            catch
              case e: java.nio.file.NoSuchFileException =>
                err(s"no file ${e.getFile()}")
              case e: LinERR => err(e.toString)
              case e         => err(s"ERR: $e\n${e.getStackTrace.mkString("\n")}")
    yield ()
