import scala.util.chaining._

case class DocCmd(
    name: String = "",
    stack: String = "",
    body: Vector[String] = Vector.empty
) {

  def md: String = s"""
## ``` $name ```

Stack: ``` $stack ```

${body.mkString}
"""

}

case class DocParser(
    xs: Vector[DocCmd] = Vector.empty,
    x: DocCmd = DocCmd(),
    com: Int = 0
) {

  def choice(l: String): DocParser = com match {
    case 1 =>
      l.trim match {
        case "*/"     => copy(com = 2)
        case s"@s $s" => copy(x = x.copy(stack = s))
        case l        => copy(x = x.copy(body = x.body :+ l))
      }
    case 2 =>
      DocParser(xs :+ x.copy(name = l.trim.pipe { case s"""case "$a"$_""" =>
        a.replace("\\\\", "\\")
      }))
    case _ =>
      l.trim match {
        case "/*" => copy(com = 1)
        case _    => this
      }
  }

  def md: String = s"""
# COMMANDS

${xs.map(_.md).mkString("\n")}
"""

}

object DocParser {

  def parse(ls: Iterable[String]): DocParser =
    ls.foldLeft(DocParser())((st, l) => st.choice(l))

}
