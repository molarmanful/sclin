package sclin

import ANY.*

class PNumSuite extends TU:

  "integers" |? (Parser.parse("1234"), LazyList(NUM(1234)))
  "decimals" |? (Parser.parse("10.5"), LazyList(NUM(10.5)))
  "no leading zero" |? (Parser.parse(".5"), LazyList(NUM(.5)))

class PStrSuite extends TU:

  "basic" |? (Parser.parse("\"asdf\""), LazyList(STR("asdf")))
  "open end" |? (Parser.parse("\"asdf"), LazyList(STR("asdf")))
  "escape" |? (Parser.parse("\"as\\df\""), LazyList(STR("as\\df")))
  "escape quote" |? (Parser.parse("\"as\\\"df\""), LazyList(STR("as\"df")))
  "open ended escape" |? (Parser.parse("\"asdf\\"), LazyList(STR("asdf\\")))

class PCmdSuite extends TU:

  "basic" |? (Parser.parse("a$df"), LazyList(CMD("a$df")))
  "brackets" |?
    (Parser.parse("[{()}]"), "[{()}]".split("").to(LazyList).map(CMD(_)))

class PDotSuite extends TU:

  "basic" |? (Parser.parse("."), LazyList(CMD(".")))
  "post-num independence" |? (Parser.parse("1."), LazyList(NUM(1), CMD(".")))
  "independence" |? (
    Parser.parse("1 .as.df"),
    LazyList(
      NUM(1), CMD("."), CMD("as"), CMD("."), CMD("df")
    )
  )

class ParseSuite extends TU:

  "empty" |? (Parser.parse(""), LazyList())
  "first line only" |? (Parser.parse("1\nignore this"), LazyList(NUM(1)))
