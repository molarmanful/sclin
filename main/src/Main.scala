import java.nio.file._
import mainargs._

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
      @arg(short = 's', doc = "Step mode.") step: Flag,
      @arg(short = 'v', doc = "Verbose mode.") verb: Flag,
      @arg(short = 'i', doc = "Implicit mode.") impl: Flag
  ) =
    def err(e: Any) = println(fansi.Color.Red(s"ERR: $e"))
    val flags       = (step.value, verb.value, impl.value)
    try
      file match
        case Some(f) => ENV.run(flags, Some(f), os.read(f))
        case _ =>
          eval match
            case Some(s) => ENV.run(flags, None, s)
            case _       => ()
    catch
      case e: NoSuchFileException => err(s"no file ${e.getFile()}")
      case e                      => err(e)

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args, allowPositional = true)
