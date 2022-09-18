type FILE = Option[os.Path]

case class PATH(f: FILE, l: Int):

  override def toString: String = s"${f.getOrElse("?")}:$l"

case class LinERR(p: PATH, t: String, x: String) extends Exception(x):

  override def toString: String = s"ERR($t): $x @ ($p)"
