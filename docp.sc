import scala.util.chaining._

case class DocCmd(
    name: String = "",
    stack: String = "",
    body: Vector[String] = Vector.empty
) {

  def md(cs: Map[String, String]): String = {
    val r = raw"#\{(.+?)\}".r
    s"""
## CMD: [``` $name ```](#${cs(name)})

Stack: ``` $stack ```

${body.map(
        r.replaceAllIn(
          _,
          m => {
            val a = m.group(1)
            s"[``` $a ```](#${cs(a)})"
          }
        )
      ).mkString("\n")}
"""
  }

}

case class DocParser(
    xs: Vector[DocCmd] = Vector.empty,
    x: DocCmd = DocCmd(),
    cs: Map[String, String] = Map.empty,
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
      val name = l.trim.pipe { case s"""case "$a"$_""" =>
        a.replace("\\\\", "\\")
      }
      val nid = "cmd-" + name.toLowerCase.replaceAll(raw"[^\w -]", "")
      def loop(id: String, n: Int = 0): String = {
        val id1 = id + s"-$n"
        if (n > 0) if (cs contains id1) loop(id, n + 1) else id1
        else if (cs contains id) loop(id, 1)
        else id
      }
      DocParser(xs :+ x.copy(name = name), cs = cs + (loop(nid) -> name))
    case _ =>
      l.trim match {
        case "/*" => copy(com = 1)
        case _    => this
      }
  }

  def md: String = {
    val cs1 = cs.map { case (a, b) => (b, a) }
    s"""
# COMMANDS

${xs.map { case DocCmd(name, _, _) => s"[``` $name ```](#${cs1(name)})" }
        .mkString("\n")}


${xs.map(_.md(cs1)).mkString("\n")}
"""
  }

}

object DocParser {

  def parse(ls: Iterable[String]): DocParser =
    ls.foldLeft(DocParser())((st, l) => st.choice(l))

}
