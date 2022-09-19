type FILE  = Option[os.Path]
type STACK = Vector[ANY]

case class PATH(f: FILE, l: Int):

  override def toString: String = s"${f.getOrElse("?")}:$l"

case class LinERR(p: PATH, t: String, x: String) extends Exception(x):

  override def toString: String = s"ERR($t): $x @ ($p)"

  override def equals(x: Any): Boolean = x match
    case x: LinERR => canEqual(x) && t == x.t

  override def hashCode: Int = t.##
