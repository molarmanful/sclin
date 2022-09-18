implicit class LIB(env: ENV):

  def out: ENV = env.arg1((x, env) =>
    print(x); env
  )

  def outn: ENV = env.arg1((x, env) =>
    println(x); env
  )

  def form: ENV = env.mod1(x => ANY.STR(x.toForm))
  def outf: ENV = env.form.outn

  def dup: ENV  = env.mods1(x => Vector(x, x))
  def dups: ENV = env.push(ANY.ARR(env.stack))
  def over: ENV = env.mods2((x, y) => Vector(x, y, x))

  def pop: ENV = env.mods1(_ => Vector())
  def clr: ENV = env.modStack(_ => Vector())
  def nip: ENV = env.mod2((x, _) => x)

  def swap: ENV = env.mods2((x, y) => Vector(y, x))
  def rev: ENV  = env.modStack(_.reverse)
  def tuck: ENV = env.mods2((x, y) => Vector(y, x, y))

  def rot: ENV  = env.mods3((x, y, z) => Vector(y, z, x))
  def rotu: ENV = env.mods3((x, y, z) => Vector(z, x, y))

  def dot: ENV = ???

  def cmd(x: String): ENV = x match

    // TYPES
    case "form" => form

    // I/O
    case ">O"  => out
    case "n>O" => outn
    case "f>O" => outf

    // STACK
    case "dup"   => dup
    case "dups"  => dups
    case "over"  => over
    case "pick"  => ???
    case "pop"   => pop
    case "clr"   => clr
    case "nip"   => nip
    case "nix"   => ???
    case "swap"  => swap
    case "rev"   => rev
    case "tuck"  => tuck
    case "trade" => ???
    case "rot"   => rot
    case "roll"  => rotu
    case "roll_" => ???
    case "dip"   => ???

    // MAGIC DOT
    case "." => dot

    case _ => throw LinERR(env.code.p, "FN", s"unknown fn \"$x\"")
