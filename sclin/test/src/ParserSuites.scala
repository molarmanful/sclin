package sclin

import ANY._

class PNumSuite extends TU:

  "integers" |? assert(Parser.parse("1234") == LazyList(NUM(1234)))
  "decimals" |? assert(Parser.parse("10.5") == LazyList(NUM(10.5)))
  "no leading zero" |? assert(Parser.parse(".5") == LazyList(NUM(.5)))

class PStrSuite extends TU:

  "basic" |? assert(Parser.parse("\"asdf\"") == LazyList(STR("asdf")))
  "open end" |? assert(Parser.parse("\"asdf") == LazyList(STR("asdf")))
  "escape" |? assert(Parser.parse("\"as\\df\"") == LazyList(STR("as\\df")))
  "escape quote" |? assert(
    Parser.parse("\"as\\\"df\"") == LazyList(STR("as\"df"))
  )
  "open ended escape" |? assert(
    Parser.parse("\"asdf\\") == LazyList(STR("asdf\\"))
  )

class PCmdSuite extends TU:

  "basic" |? assert(Parser.parse("a$df") == LazyList(CMD("a$df")))
  "brackets" |? assert(
    Parser.parse("[{()}]") == "[{()}]".split("").to(LazyList).map(CMD(_))
  )

class PDotSuite extends TU:

  "basic" |? assert(Parser.parse(".") == LazyList(CMD(".")))
  "post-num independence" |? assert(
    Parser.parse("1.") == LazyList(NUM(1), CMD("."))
  )
  "independence" |? assert(
    Parser.parse("1 .as.df") == LazyList(
      NUM(1),
      CMD("."),
      CMD("as"),
      CMD("."),
      CMD("df")
    )
  )

class ParseSuite extends TU:

  "empty" |? assert(Parser.parse("") == LazyList())
  "first line only" |? assert(
    Parser.parse("1\nignore this") == LazyList(NUM(1))
  )
