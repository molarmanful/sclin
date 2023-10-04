package sclin

import ANY._

class NumSuite extends munit.FunSuite:

  test("integers")(assertEquals(Parser.parse("1234"), LazyList(NUM(1234))))
  test("decimals")(assertEquals(Parser.parse("10.5"), LazyList(NUM(10.5))))
  test("no leading zero")(
    assertEquals(Parser.parse(".5"), LazyList(NUM(.5)))
  )

class StrSuite extends munit.FunSuite:

  test("basic")(
    assertEquals(Parser.parse("\"asdf\""), LazyList(STR("asdf")))
  )
  test("open end")(
    assertEquals(Parser.parse("\"asdf"), LazyList(STR("asdf")))
  )
  test("escape")(
    assertEquals(Parser.parse("\"as\\df\""), LazyList(STR("as\\df")))
  )
  test("escape quote")(
    assertEquals(Parser.parse("\"as\\\"df\""), LazyList(STR("as\"df")))
  )

class CmdSuite extends munit.FunSuite:

  test("basic")(
    assertEquals(Parser.parse("a$df"), LazyList(CMD("a$df")))
  )
  test("brackets")(
    assertEquals(
      Parser.parse("[{()}]"),
      "[{()}]".split("").to(LazyList).map(CMD(_))
    )
  )

class DotSuite extends munit.FunSuite:

  test("basic")(assertEquals(Parser.parse("."), LazyList(CMD("."))))
  test("post-num independence")(
    assertEquals(Parser.parse("1."), LazyList(NUM(1), CMD(".")))
  )
  test("independence")(
    assertEquals(
      Parser.parse("1 .as.df"),
      LazyList(NUM(1), CMD("."), CMD("as"), CMD("."), CMD("df"))
    )
  )
