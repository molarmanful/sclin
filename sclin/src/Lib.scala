package sclin

import monix.eval.Task
import scala.annotation._
import scala.collection.immutable.VectorMap
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.StdIn._
import scala.util.chaining._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import spire.algebra._
import spire.implicits._
import spire.math._
import upickle.default._
import ANY._
import Lambda._

extension (env: ENV)

  def cmd(x: String): ENV = x match

    case s"#$k" if k != "" => env
    case s"\$k" if k != "" => env.push(CMD(k).toFN(env))

    case s"`$k" if k != "" =>
      def loop(
          n: Int = env.code.p.l + 1,
          res: ARRW[String] = Vector()
      ): (ARRW[String], Int) =
        env.lines.get(PATH(env.code.p.f, n)) match
          case Some(STR(s), f) if !s.trim.startsWith(k) => loop(n + 1, res :+ s)
          case _                                        => (res, n)
      val (x, i) = loop()
      env.push(STR(x.mkString("\n"))).push(NUM(i)).evalLine
    case "`" => env

    case s"=$$$$$k" if k != "" => env.arg1((v, env) => env.addGlob(k, v))
    case s"=$$$k" if k != ""   => env.arg1((v, env) => env.addLoc(k, v))
    // TODO:
    // case s"%$$$$$k" if k != "" => env.arg1((v, env) => env.addGlob(k, v))
    // case s"%$$$k" if k != ""   => env.arg1((v, env) => env.addLoc(k, v))
    case s"$$$$$k" if k != "" =>
      env.getGlob(k) match
        case None    => cmd1(x)
        case Some(v) => env.push(v)
    case s"$$$k" if k != "" =>
      env.getLoc(k) match
        case None    => cmd1(x)
        case Some(v) => env.push(v)

    case _ =>
      if env.scope.contains(x) then env.push(env.scope(x)).eval
      else if env.ids.contains(x) then env.push(NUM(env.ids(x).l)).evalLine
      else if env.gscope.contains(x) then env.push(env.gscope(x)).eval
      else if env.gids.contains(x) then env.push(NUM(env.gids(x).l)).evalLine
      else cmd1(x)

  def cmd1(x: String): ENV = (x: @switch) match

    // CMDOC START

    case "("   => startFN
    case ")"   => env
    case ")~"  => evalTASK
    case ")!"  => evalTRY
    case ")#"  => locId
    case ")##" => globId
    case ")="  => lambda
    case "["   => startARR
    case "]"   => endARR
    case "{"   => startARR
    case "}"   => endMAP
    case "."   => dot

    /*
    @s a -> STR
    Type of `a`.
     */
    case "type" => getType
    /*
    @s a -> STR
    `a` as formatted string.
     */
    case "form" => form
    /*
    @s a -> STR
    Converts `a` to `SEQ`.
     */
    case ">Q" => toSEQ
    /*
    @s a -> ARR
    Converts `a` to `ARR`.
     */
    case ">A" => toARR
    /*
    @s a -> ARR
    Converts `a` to `MAP`.
     */
    case ">M" => toMAP
    /*
    @s a' -> STR'
    Atomic #{>S}.
     */
    case "S" => vSTR
    /*
    @s a -> STR
    Converts `a` to `STR`.
     */
    case ">S" => toSTR
    /*
    @s a -> NUM
    Converts `a` to `NUM`.
     */
    case ">N" => toNUM
    /*
    @s a' -> NUM'
    Atomic #{>N}.
     */
    case "N" => vNUM
    /*
    @s a' -> DBL'
    Atomic #{>D}.
     */
    case "D" => vDBL
    /*
    @s a -> DBL
    Converts `a` to `DBL`.
     */
    case ">D" => toDBL
    /*
    @s a -> FN
    Converts `a` to `FN`.
     */
    case ">F" => toFN
    /*
    @s (a >STR) (b >STR) -> ERR
    Converts `a` to `ERR` with message `b`.
     */
    case ">E" => toERR
    /*
    @s a -> TASK
    Converts `a` to `TASK`.
     */
    case ">~" => toTASK
    /*
    @s a -> FUT'
    Converts `a` to `FUT`.
     */
    case "~>" => toFUT
    /*
    @s a -> TRY
    Converts `a` to `TRY`.
     */
    case ">!?" => toTRY
    /*
    @s a -> TF
    Converts `a` to `TF`.
     */
    case ">?" => toTF
    /*
    @s (a >NUM) (b >NUM)' -> STR
    Converts `a` to an `STR` formatted to `b`'s specifications.
     */
    case "N>d" => toNUMD
    /*
    @s a b -> _
    Converts `a` to type of `b`.
     */
    case ">TT" => matchType
    /*
    @s (a >STR)' -> MAP
    #{>M} using a multiline string.
    Each line of `a` is #{#}ed, and the resulting top 2 stack items form each key-value pair.
    ```sclin
    `` >>M
    "a" 1
    "b" 2
    "c" 3
    `
    ```
     */
    case ">>M" => lineMAP
    /*
    @s (a >STR)' -> _
    Converts `a` from JSON to `ANY`.
    ```sclin
    g; js>
    {"a": 1, "b": 2, "c": [3, 4]}
    ```
     */
    case "js>" => fromJSON
    /*
    @s a -> STR
    Converts `a` from `ANY` to JSON.
    ```sclin
    {"a" 1, "b" 2, "c" [3 4] , } >js
    ```
     */
    case ">js" => toJSON

    /*
    @s -> UN
    `UN`
     */
    case "UN" => env.push(UN)
    /*
    @s -> TF
    True.
     */
    case "$T" => env.push(TF(true))
    /*
    @s -> TF
    False.
     */
    case "$F" => env.push(TF(false))
    /*
    @s -> FN
    Empty `FN`.
     */
    case "()" => env.push(UN.toFN(env))
    /*
    @s -> ARR
    Empty `ARR`.
     */
    case "[]" => env.push(UN.toARR)
    /*
    @s -> MAP
    Empty `MAP`.
     */
    case "{}" => env.push(UN.toMAP)
    /*
    @s -> TASK
    Empty `TASK`.
     */
    case "()~" => env.push(UN.toTASK)
    /*
    @s -> TRY
    Empty `TRY`.
     */
    case "()!" => env.push(UN.toTRY)
    /*
    @s -> NUM
    π (Pi).
     */
    case "$PI" => env.push(NUM(Real.pi))
    /*
    @s -> NUM
    e (Euler's number).
     */
    case "$E" => env.push(NUM(Real.e))
    /*
    @s -> NUM
    Φ (Golden Ratio).
     */
    case "$PHI" => env.push(NUM(Real.phi))
    /*
    @s -> NUM
    Uniformly random number.
     */
    case "$rng" => env.push(NUM(random))
    /*
    @s -> NUM
    Current line number of program execution.
     */
    case "$LINE" => getLNum
    /*
    @s -> STR
    Current file of program execution.
     */
    case "$FILE" => getLFile
    /*
    @s -> SEQ[NUM*]
    Infinite `SEQ` of 0 to ∞.
     */
    case "$W" => env.push(LazyList.from(0).map(NUM(_)).toSEQ)
    /*
    @s -> SEQ[NUM*]
    Infinite `SEQ` of 1 to ∞.
     */
    case "$N" => env.push(LazyList.from(1).map(NUM(_)).toSEQ)
    /*
    @s -> SEQ[NUM*]
    Infinite `SEQ` of primes.
     */
    case "$P" => env.push(prime.lazyList.map(NUM(_)).toSEQ)
    /*
    @s -> ARR[STR*]
    `ARR` of lines of currently-executing file.
     */
    case "$L*"  => getLns
    case "$ENV" => getSc
    /*
    @s -> STR
    `UPPERCASE` alphabet.
     */
    case "$ABC" => env.push(STR("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    /*
    @s -> STR
    `lowercase` alphabet.
     */
    case "$abc" => env.push(STR("abcdefghijklmnopqrstuvwxyz"))
    /*
    @s -> STR | UN
    Current line.
     */
    case "g@" => getLHere
    /*
    @s -> STR | UN
    Next line.
     */
    case "g;" => getLNext
    /*
    @s -> STR | UN
    Previous line.
     */
    case "g;;" => getLPrev
    /*
    @s -> STR
    Newline character.
     */
    case "n\\" => env.push(STR("\n"))

    /*
    @s (a >FN)' ->
    Loads ID `a` into local scope.
    ```sclin
    "outer"=$a ( \a @$ a ) # $a
    #a "inner"
    ```
     */
    case "@$" => locId
    /*
    @s (a >FN)' ->
    Loads ID `a` into global scope.
    ```sclin
    \a @$$ ( "inner" =$a $a ) # a
    #a "outer"
    ```
     */
    case "@$$" => globId
    /*
    @s _* (a >FN) -> _*
    Stores stack items into local variables defined by `a`.
    Somewhat analogous to function arguments in other languages.
    ```sclin
    1 2 3 ;
    ( a b c ) -> $c $b $a
    ```
     */
    case "->" => lambda

    /*
    @s -> STR
    Line from STDIN.
     */
    case "i>" => in
    /*
    @s (a >STR) ->
    Sends `a` to STDOUT.
     */
    case ">o" => out
    /*
    @s (a >STR) ->
    #{>o}s `a` with trailing newline.
     */
    case "n>o" => outn
    /*
    @s a ->
    #{form}s and #{n>o}s `a`.
     */
    case "f>o" => outf
    /*
    @s a -> a a
     */
    case "dup" => dup
    /*
    @s a* -> a* ARR[a*]
     */
    case "dups" => dups
    /*
    @s a b -> a a b
     */
    case "dupd" => dupd
    /*
    @s a b -> a b a
     */
    case "over" => over
    /*
    @s a b -> a b a b
     */
    case "ddup" => ddup
    /*
    @s a b c -> a b c a b c
     */
    case "edup" => edup
    /*
    @s (a @ n) b* (n >NUM) -> a b* a
    #{dup}s `n`th item from top of stack.
    ```sclin
    4 3 2 1 0 3pick
    ```
    ```sclin
    4 3 2 1 0 1_ pick
    ```
     */
    case "pick" => pick
    /*
    @s _ ->
     */
    case "pop" => pop
    /*
    @s _* ->
     */
    case "clr" => clr
    /*
    @s _ b -> b
     */
    case "nip" => nip
    /*
    @s _ _ ->
     */
    case "ppop" => pop.pop
    /*
    @s _ _ _ ->
     */
    case "qpop" => pop.pop.pop
    /*
    @s (a @ n) b* (n >NUM) -> _*
    #{pop}s `n`th item from top of stack.
     */
    case "nix" => nix
    /*
    @s a b -> b a
     */
    case "swap" => swap
    /*
    @s a* -> _*
    Reverses stack.
     */
    case "rev" => rev
    /*
    @s a b c -> b a c
     */
    case "swapd" => swapd
    /*
    @s a b -> b a b
     */
    case "tuck" => tuck
    /*
    @s (a @ n) b* c (n >NUM) -> c b* a
    #{swap}s `c` with `n`th item from top of stack.
    ```sclin
    4 3 2 1 0 3trade
    ```
    ```sclin
    4 3 2 1 0 1_ trade
    ```
     */
    case "trade" => trade
    /*
    @s a b c -> b c a
     */
    case "rot" => rot
    /*
    @s a b c -> c a b
     */
    case "rot_" => rotu
    /*
    @s (a @ n) b* (n >NUM) -> b* a
    #{rot}s to top `n`th item from top of stack.
    ```sclin
    4 3 2 1 0 3roll
    ```
    ```sclin
    4 3 2 1 0 1_ roll
    ```
     */
    case "roll" => roll
    /*
    @s b* c (n >NUM) -> (c @ n) b*
    #{rot_}s `c` to `n`th from top of stack.
    ```sclin
    4 3 2 1 0 3roll_
    ```
    ```sclin
    4 3 2 1 0 1_ roll_
    ```
     */
    case "roll_" => rollu
    /*
    @s a* b (f >FN) -> _* b
    #{pop}s `b`, #{#}s `f`, and pushes `b`.
     */
    case "dip" => dip
    /*
    @s a -> FN[a]
    Wraps `a` in `FN`.
     */
    case "\\" => wrapFN
    /*
    @s a* f -> _*
    Executes `f`.
    ```sclin
    1 2 ( 3 + 4 ) #
    ```
     */
    case "#" => eval
    /*
    @s f' -> _'
    Evaluates `f` (#{#} but only preserves resulting top of stack).
    ```sclin
    1 2 ( dups 3+` ) Q
    ```
     */
    case "Q" => quar
    /*
    @s a* (n >NUM) -> _*
    #{#}s `n`th line.
     */
    case "@@" => evalLine
    /*
    @s a* (n >NUM) -> _*
    #{#}s `n`th line relative to current line.
     */
    case "@~" => evalLRel
    /*
    @s a* -> _*
    #{#}s current line.
     */
    case "@" => evalLHere
    /*
    @s a* -> _*
    #{#}s next line.
     */
    case ";" => evalLNext
    /*
    @s a* -> _*
    #{#}s previous line.
     */
    case ";;" => evalLPrev
    /*
    @s (n >NUM) -> STR | UN
    `n`th line.
     */
    case "g@@" => getLn
    /*
    @s (n >NUM) -> STR | UN
    `n`th line relative to current line.
     */
    case "g@~" => getLRel
    /*
    @s a* (b >TF) f -> _*
    #{#}s `f` if `b` is truthy.
     */
    case "&#" => evalAnd
    /*
    @s a* (b >TF) f -> _*
    #{#}s `f` if `b` is falsy.
     */
    case "|#" => evalOr
    /*
    @s a* (b >TF) f g -> _*
    #{#}s `f` if `b` is truthy; else #{#}s `g`.
     */
    case "?#" => evalIf
    /*
    @s a* (b >MAP) -> _*
    Iterates through each key-value pair of `b`.
    For each pair: if the #{Q} of the key is truthy,
    then #{#}s the value and short-circuits.
     */
    case "??#" => evalIf$
    /*
    @s a* f (n >NUM) -> _*
    #{#}s `f` `n` times.
     */
    case "*#" => evalTimes
    /*
    @s a* f g -> _*
    Tries to #{#} `f`; on error, pushes caught `ERR` and #{#}s `g`.
     */
    case "!#" => evalTry
    /*
    @s f' -> TRY'
    #{Q}s `f` and wraps the result in a `TRY`.
     */
    case "!Q" => evalTRY
    /*
    @s f' -> TASK'
    #{Q}s `f` asynchronously, returning a future.
     */
    case "~Q" => evalTASK
    /*
    @s (e ERR) ->
    Throws `e`.
     */
    case ">!" => throwERR
    /*
    @s (a >ARR) f -> ARR
    #{#}s `f` on `a` as if it were a stack.
    ```sclin
    [1 2 3 4] ( 5 swap ) '
    ```
     */
    case "'" => evalArrSt
    /*
    @s (a* >ARR) f -> _*
    #{#}s `f` on the stack as if it were an `ARR`.
    ```sclin
    1 2 3 4 1.+.map '_
    ```
     */
    case "'_" => evalStArr
    /*
    @s ->
    Clears code queue, similar to the "break" keyword in other languages.
     */
    case "end" => env.copy(code = FN(env.code.p, LazyList()))

    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a * 10 ^ b`
     */
    case "E" => scale
    /*
    @s (a >NUM)' -> NUM'
    Rounds `a` towards 0.
     */
    case "I" => trunc
    /*
    @s (a >NUM)' -> TF'
    Whether `a` is an integer.
     */
    case "I?" => isInt
    /*
    @s (a >NUM)' -> NUM'
    Rounds `a` towards -∞.
     */
    case "|_" => floor
    /*
    @s (a >NUM)' -> NUM'
    Rounds `a` to nearest integer.
     */
    case "|~" => round
    /*
    @s (a >NUM)' -> NUM'
    Rounds `a` towards ∞.
     */
    case "|^" => ceil
    /*
    @s (a >NUM)' (b >NUM)' -> ARR[NUM*]'
    Converts `a` from decimal to `ARR` of base-`b` digits.
    ```sclin
    153 2X>b
    ```
    ```sclin
    153 16X>b
    ```
     */
    case "X>b" => fromDec
    /*
    @s (a >ARR[>NUM*]) (b >NUM)' -> NUM'
    Converts base-`b` digits to decimal.
    ```sclin
    [1 0 0 1 1 0 0 1] 2b>X
    ```
    ```sclin
    [9 9] 16b>X
    ```
     */
    case "b>X" => toDec
    /*
    @s (a >NUM)' -> ARR[NUM NUM]'
    Converts `a` to a numerator-denominator pair.
    ```sclin
    4 6/ >n/d
    ```
    ```sclin
    $PI >n/d
    ```
     */
    case ">n/d" => toNumDen
    /*
    @s (a >NUM)' -> ARR[NUM NUM]'
    Whether `a` is an exact value (i.e. represented in full precision).
    ```sclin
    2 3/ prec?
    ```
    ```sclin
    $PI prec?
    ```
     */
    case "prec?" => isExact

    /*
    @s (a >NUM)' -> NUM'
    `-a`
     */
    case "_" => neg
    /*
    @s (a >STR)' -> STR'
    Atomic #{_`}.
     */
    case "__" => neg$
    /*
    @s a -> _
    Reverses `a`.
     */
    case "_`" => neg$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a + b`
     */
    case "+" => add
    /*
    @s (a >STR)' (b >STR)' -> STR'
    Atomic #{+`}.
     */
    case "++" => add$
    /*
    @s a b -> _
    Concatenates `a` and `b`.
     */
    case "+`" => add$$
    /*
    @s a b -> _
    Prepends `a` to `b`.
     */
    case "<+" => cons
    /*
    @s a b -> _
    Appends `b` to `a`.
     */
    case "+>" => snoc
    // TODO: docs
    case "+<" => uncons
    case ">+" => unsnoc
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a - b`
     */
    case "-" => sub
    /*
    @s (a >STR)' (b >STR)' -> STR'
    Atomic #{-`}.
     */
    case "--" => sub$
    /*
    @s a b -> _
    Remove occurrences of `b` from `a`.
    If `a` is `MAP`, then removal is performed on keys instead of values.
    ```sclin
    [1 2 3 4] 2-`
    ```
    ```sclin
    {0 1, 2 3, } 2-`
    ```
     */
    case "-`" => sub$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a * b`
     */
    case "*" => mul
    /*
    @s (a >STR)' (b >NUM)' -> STR'
    Atomic #{*`}.
     */
    case "**" => mul$
    /*
    @s a b -> _
    `a` replicated according to `b`.
    If `b` is iterable, then `a` and `b` are recursively zipped together and replicated.
    ```sclin
    [1 2 3 4] [0 2 0 3] *` >A
    ```
    ```sclin
    [1 2 3 4] 3*` >A
    ```
     */
    case "*`" => mul$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a / b`. Throws error if `b` is 0.
     */
    case "/" => div
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Integer #{/}.
     */
    case "/~" => divi
    /*
    @s (a >STR)' (b >NUM)' -> SEQ[STR*]'
    Atomic #{/`}.
     */
    case "//" => div$
    /*
    @s a (b >NUM)' -> SEQ
    `a` chunked to size `b`.
    ```sclin
    [1 2 3 4 5] 2/` >A
    ```
     */
    case "/`" => div$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a (mod b)`
     */
    case "%" => mod
    /*
    @s (a >NUM)' (b >NUM)' -> NUM' NUM'
    Results of #{/~} and #{%} on `a` and `b`.
     */
    case "/%" => divmod
    /*
    @s (a >STR)' (b >NUM)' -> SEQ[STR*]'
    Atomic #{%`}.
     */
    case "%%" => mod$
    /*
    @s a (b >NUM)' -> SEQ
    `a` windowed to size `b`.
    ```sclin
    [1 2 3 4 5] 3%` >A
    ```
     */
    case "%`" => mod$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a ^ b`. Throws error if result would be a complex number.
     */
    case "^" => pow
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    #{^} but `b` is coerced to `int`.
     */
    case "^~" => powi
    /*
    @s (a >STR)' (b >NUM)' -> SEQ[STR*]'
    Atomic #{^`}.
     */
    case "^^" => pow$
    /*
    @s a (n >NUM)' -> SEQ'
    Cartesian power of seed `a` to `n`.
    ```sclin
    "abc" 3^` >A
    ```
     */
    case "^`" => pow$$

    /*
    @s (a >NUM)' -> NUM'
    `e ^ a`
     */
    case "e^" => exp
    /*
    @s (a >NUM)' -> NUM'
    Absolute value of `a`.
     */
    case "abs" => abs
    /*
    @s (a >NUM)' -> NUM'
    Sine of `a`.
     */
    case "sin" => sin
    /*
    @s (a >NUM)' -> NUM'
    Cosine of `a`.
     */
    case "cos" => cos
    /*
    @s (a >NUM)' -> NUM'
    Tangent of `a`.
     */
    case "tan" => tan
    /*
    @s (a >NUM)' -> NUM'
    Arcsine of `a`.
     */
    case "sin_" => asin
    /*
    @s (a >NUM)' -> NUM'
    Arccosine of `a`.
     */
    case "cos_" => acos
    /*
    @s (a >NUM)' -> NUM'
    Arctangent of `a`.
     */
    case "tan_" => atan
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Arctangent of `a` with `b` as quadrant.
     */
    case "tan_II" => atan2
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic sine of `a`.
     */
    case "sinh" => sinh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic cosine of `a`.
     */
    case "cosh" => cosh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic tangent of `a`.
     */
    case "tanh" => tanh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic arcsine of `a`.
     */
    case "sinh_" => asinh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic arccosine of `a`.
     */
    case "cosh_" => acosh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic arctangent of `a`.
     */
    case "tanh_" => atanh
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Base `b` logarithm of `a`.
     */
    case "log" => log
    /*
    @s (a >NUM)' -> NUM'
    Natural logarithm of `a`.
     */
    case "ln" => ln
    /*
    @s (a >NUM)' -> NUM'
    Base-10 logarithm of `a`.
     */
    case "logX" => log10
    /*
    @s (a >NUM)' -> NUM'
    Whether `a` is prime. Uses a strong pseudo-primality test with a 1/1e12 chance of being wrong.
     */
    case "P?" => isPrime
    /*
    @s (a >NUM)' -> MAP[(NUM => NUM)*]
    Prime-factorizes `a` into pairs of prime `y` and frequency `z`.
    ```sclin
    340P/
    ```
     */
    case "P/" => factor

    /*
    @s (a >TF)' -> TF'
    Atomic #{!`}.
     */
    case "!" => not
    /*
    @s (a >TF) -> TF
    Logical NOT.
     */
    case "!`" => not$$
    /*
    @s a' b' -> (a | b)'
    Atomic #{&`}.
     */
    case "&" => min
    /*
    @s (a >TF)' (b >TF)' -> TF'
    Atomic #{&&`}.
     */
    case "&&" => and
    /*
    @s a b -> a | b
    Minimum of `a` and `b`.
     */
    case "&`" => min$$
    /*
    @s (a >TF) (b >TF) -> TF
    Logical AND of `a` and `b`.
     */
    case "&&`" => and$$
    /*
    @s a' b' -> (a | b)'
    Atomic #{|`}.
     */
    case "|" => max
    /*
    @s (a >TF)' (b >TF)' -> TF'
    Atomic #{||`}.
     */
    case "||" => or
    /*
    @s a b -> a | b
    Maximum of `a` and `b`.
     */
    case "|`" => max$$
    /*
    @s (a >TF) (b >TF) -> TF
    Logical OR of `a` and `b`.
     */
    case "||`" => or$$
    /*
    @s a' b' -> (-1 | 0 | 1)'
    Atomic #{<=>`}.
     */
    case "<=>" => cmp
    /*
    @s a b -> -1 | 0 | 1
    Comparison (-1, 0, or 1 depending on whether `a` is less than, equal to, or greater than `b`).
     */
    case "<=>`" => cmp$$
    /*
    @s a' b' -> TF'
    Atomic #{=`}.
     */
    case "=" => eql
    /*
    @s a b -> TF
    Whether `a` equals `b`.
     */
    case "=`" => eql$$
    /*
    @s a' b' -> TF'
    Atomic #{!=`}.
     */
    case "!=" => neq
    /*
    @s a b -> TF
    Whether `a` does not equals `b`.
     */
    case "!=`" => neq$$
    /*
    @s a' b' -> TF'
    Atomic #{<`}.
     */
    case "<" => lt
    /*
    @s a b -> TF
    Whether `a` is less than `b`.
     */
    case "<`" => lt$$
    /*
    @s a' b' -> TF'
    Atomic #{>`}.
     */
    case ">" => gt
    /*
    @s a b -> TF
    Whether `a` is greater than `b`.
     */
    case ">`" => gt$$
    /*
    @s a' b' -> TF'
    Atomic #{<=`}.
     */
    case "<=" => lteq
    /*
    @s a b -> TF
    Whether `a` is less than or equal to `b`.
     */
    case "<=`" => gt$$
    /*
    @s a' b' -> TF'
    Atomic #{>=`}.
     */
    case ">=" => gteq
    /*
    @s a b -> TF
    Whether `a` is greater than or equal to `b`.
     */
    case ">=`" => gt$$

    /*
    @s a i' -> (a._ | UN)'
    Value at atomic index `i` in `a`.
     */
    case ":" => get
    /*
    @s a -> a._
    Value at random index in `a`.
     */
    case ":r" => getr
    /*
    @s a i -> a._ | UN
    Value at index `i` in `a`.
     */
    case ":`" => get$$
    /*
    @s a b (i >SEQ) -> x
    #{:`} with `i` folded over `a`.
     */
    case ":/" => gets
    /*
    @s a b i -> x
    Sets value at index `i` in `a` to `b`.
     */
    case ":=" => set
    /*
    @s a b (i >SEQ) -> x
    #{:=} with `i` folded over `a`.
     */
    case ":/=" => sets
    /*
    @s a f i -> x
    Modifies value at index `i` using `f`.
     */
    case ":%" => setmod
    /*
    @s a b (i >SEQ) -> x
    #{:%} with `i` folded over `a`.
     */
    case ":/%" => setmods
    /*
    @s a i -> x
    Removes index `i` from `a`.
     */
    case ":-" => idel
    /*
    @s a b' -> TF'
    Whether `a` has atomic `b`.
     */
    case ":?" => has
    /*
    @s a b -> TF
    Whether `a` has `b`.
    `MAP`s check `b` against keys; other types of `a` check `b` against values.
     */
    case ":?`" => has$$
    /*
    @s a -> NUM
    Length of `a`.
     */
    case "len" => len
    /*
    @s a b -> ARR[a b]
    Pairs `a` and `b` in an `ARR`.
     */
    case "," => wrap$
    /*
    @s a -> ARR[a]
    Wraps `a` in an `ARR`.
     */
    case ",," => wrap
    /*
    @s a' b' -> ARR[a b]'
    Vectorized #{,}.
     */
    case ",'" => wrapv$
    /*
    @s a' -> ARR[a]'
    Vectorized #{,,}.
     */
    case ",,'" => wrapv
    /*
    @s a* -> a
    Wraps stack in an `ARR`.
     */
    case ",`" => wrap$$
    /*
    @s a -> a*
    Unwraps `a`.
     */
    case ",_" => unwrap
    /*
    @s _* a -> a*
    Replaces stack with `a` unwrapped.
     */
    case ",,_" => unwrap$
    /*
    @s a (n >NUM)' -> _
    Takes up to `n` items from `a`.
    Negative `n` takes from the end instead of the start.
     */
    case "tk" => tk
    /*
    @s a (n >NUM)' -> _
    Drops up to `n` items from `a`.
    Negative `n` drops from the end instead of the start.
     */
    case "dp" => dp
    /*
    @s a -> _
    Flattens `a` by one depth.
     */
    case "flat" => flat
    /*
    @s a -> _
    Flattens `a` recursively.
     */
    case "rflat" => rflat
    /*
    @s a -> SEQ
    Infinite `SEQ` with `a` repeated.
    ```sclin
    5rep 10tk >A
    ```
    ```sclin
    [1 2 3] rep 10tk >A
    ```
     */
    case "rep" => rep
    /*
    @s a -> SEQ
    Infinite `SEQ` with elements of `a` cycled.
    ```sclin
    [1 2 3] cyc 10tk >A
    ```
     */
    case "cyc" => cyc
    /*
    @s (a >NUM)' -> ARR[1*]'
    Length-`a` `ARR` of 1's.
    ```sclin
    10I*
    ```
     */
    case "I*" => ones
    /*
    @s (a >ARR) -> ARR
    `ARR` of 1's with dimensions `a`.
    ```sclin
    [2 3 4] I^
    ```
     */
    case "I^" => one$
    /*
    @s a b -> _
    Convert the shape of `a` to the shape of `b`.
    ```sclin
    $W [2 3 4] I^ mold
    ```
    ```sclin
    $W [1 2 3] I* mold
    ```
     */
    case "mold" => toShape
    /*
    @s a (f: b -> _) -> SEQ
    Infinite `SEQ` of `f` successively #{Q}ed to `a`.
    ```sclin
    1 1.+ itr 10tk >A
    ```
    ```sclin
    1 ( 1+ 1 swap / ) itr 10tk >A
    ```
     */
    case "itr" => itr
    /*
    @s a (f: b -> _ _ | ) -> SEQ
    `SEQ` generated from `f` successively #{Q}ed to `a`,
    where `x` is the new current item and `y` is the next `b` to be subsequently #{Q}ed to `f`.
    Generation stops if `f` #{Q}ed to `a` results in an empty stack.
    ```sclin
    0 1, ( ,_ tuck + dups \swap dip ) fold_ 10tk >A
    ```
     */
    case "fold_" => unfold
    /*
    @s a -> (SEQ | ARR)[ARR[k v]*]
    `SEQ` of key/value pairs in `a`.
    ```sclin
    ["a" "b" "c" "d"] >kv >A
    ```
    ```sclin
    {"x""a", "y""b", "z""c", } >kv >A
    ```
     */
    case ">kv" => enumL
    /*
    @s a -> MAP[(_ => _)*]
    #{>kv} and #{>M}.
    ```sclin
    ["a" "b" "c" "d"] =>kv
    ```
     */
    case "=>kv" => enumL.toMAP
    /*
    @s a -> SEQ | ARR
    Keys in `a`.
    ```sclin
    {"x" "a", "y" "b", "z" "c", } >k >A
    ```
     */
    case ">k" => keys
    /*
    @s a -> SEQ | ARR
    Values in `a`.
    ```sclin
    {"x""a", "y""b", "z""c", } >v >A
    ```
     */
    case ">v" => vals
    /*
    @s (a >NUM)' (b >NUM)' -> ARR[NUM*]'
    Exclusive range from `a` to `b`.
     */
    case "a>b" => range
    /*
    @s (a >NUM)' (b >NUM)' -> ARR[NUM*]'
    Inclusive range from `a` to `b`.
     */
    case "a-b" => irange
    /*
    @s (a >NUM)' -> ARR[NUM*]'
    Exclusive range from 0 to `a`.
     */
    case "O>a" => env.push(NUM(0)).swap.range
    /*
    @s (a >NUM)' -> ARR[NUM*]'
    Inclusive range from 0 to `a`.
     */
    case "O-a" => env.push(NUM(0)).swap.irange
    /*
    @s (a >NUM)' -> ARR[NUM*]'
    Exclusive range from `a` to 0.
     */
    case "a>O" => env.push(NUM(0)).range
    /*
    @s (a >NUM)' -> ARR[NUM*]'
    Inclusive range from `a` to 0.
     */
    case "a-O" => env.push(NUM(0)).irange
    /*
    @s (a >NUM)' -> ARR[NUM*]'
    Exclusive range from 1 to `a`.
     */
    case "I>a" => env.push(NUM(1)).swap.range
    /*
    @s (a >NUM)' -> ARR[NUM*]'
    Inclusive range from 1 to `a`.
     */
    case "I-a" => env.push(NUM(1)).swap.irange
    /*
    @s (a >NUM)' -> ARR[NUM*]'
    Exclusive range from `a` to 1.
     */
    case "a>I" => env.push(NUM(1)).range
    /*
    @s (a >NUM)' -> ARR[NUM*]'
    Inclusive range from `a` to 1.
     */
    case "a-I" => env.push(NUM(1)).irange
    /*
    @s a -> _
    Shuffles `a`.
    ```sclin
    10O>a shuf
    ```
     */
    case "shuf" => shuffle
    /*
    @s a -> SEQ
    All permutations of `a`.
    ```sclin
    [1 2 3] perm >A
    ```
     */
    case "perm" => perm
    /*
    @s a (n >NUM)' -> SEQ'
    All length-`n` combinations of `a`.
    ```sclin
    [1 2 3] 2comb >A
    ```
     */
    case "comb" => comb
    /*
    @s a -> SEQ
    All subsets of `a`.
    ```sclin
    [1 2 3] ^set >A
    ```
     */
    case "^set" => powset
    /*
    @s a[_*] -> SEQ'
    Cartesian product of iterable-of-iterables `a` to `n`.
    ```sclin
    ["abc" "def" "ghi"] Q* >A
    ```
     */
    case "Q*" => cProd
    /*
    @s a[_*] -> _[_*]
    Transposes a collection of collections matrix-style.
    ```sclin
    [[1 2 3][4 5 6][7 8 9]] tpose
    ```
    ```sclin
    [[1 2][3 4 5][6]] tpose
    ```
     */
    case "tpose" => tpose
    /*
    @s (a >STR)' (b >NUM)' (c >STR)' -> STR'
    Atomic #{pad`}.
     */
    case "pad" => pad
    /*
    @s (a >STR)' (b >NUM)' (c >STR)' -> STR'
    Atomic #{padl`}.
     */
    case "padl" => padl
    /*
    @s (a >STR)' (b >NUM)' (c >STR)' -> STR'
    Atomic #{padc`}.
     */
    case "padc" => padc
    /*
    @s a[_*] (b >NUM)' c -> STR'
    Pads `a` from the right to length `b` using `c`.
    ```sclin
    [1 2 3 4] 9 0pad`
    ```
    ```sclin
    [1 2 3 4] 9 [5 6 7] pad`
    ```
    ```sclin
    [1 2 3 4] 3 0pad`
    ```
     */
    case "pad`" => pad$
    /*
    @s a[_*] (b >NUM)' c -> STR'
    Pads `a` from the right to length `b` using `c`.
    ```sclin
    [1 2 3 4] 9 0padl`
    ```
    ```sclin
    [1 2 3 4] 9 [5 6 7] padl`
    ```
    ```sclin
    [1 2 3 4] 3 0padl`
    ```
     */
    case "padl`" => padl$
    /*
    @s a[_*] (b >NUM)' c -> STR'
    Pads `a` from the right to length `b` using `c`.
    ```sclin
    [1 2 3 4] 9 0padc`
    ```
    ```sclin
    [1 2 3 4] 9 [5 6 7] padc`
    ```
    ```sclin
    [1 2 3 4] 3 0padc`
    ```
     */
    case "padc`" => padc$

    /*
    @s (a >STR)' -> ARR[NUM*]'
    Converts `a` to codepoints.
    ```sclin
    "hello"S>c
    ```
     */
    case "S>c" => toCodePt
    /*
    @s (a >ARR[NUM*]) -> STR
    Converts iterable of codepoints to `STR`.
    ```sclin
    [104 101 108 108 111] c>S
    ```
     */
    case "c>S" => fromCodePt
    /*
    @s (a >STR)' (b >STR)' -> ARR'
    Splits `a` with `b`.
     */
    case "<>" => split
    /*
    @s a (i >NUM) -> ARR[_ _]
    #{tk} and #{dp} of `a` at index `i`.
     */
    case "<>:" => splitAt
    /*
    @s (a >STR)' -> ARR'
    #{<>}s with empty string.
     */
    case "c<>" => env.push(STR("")).split
    /*
    @s (a >STR)' -> ARR'
    #{<>}s with space.
     */
    case "w<>" => env.push(STR(" ")).split
    /*
    @s (a >STR)' -> ARR'
    #{<>}s with newline.
     */
    case "n<>" => env.push(STR("\n")).split
    /*
    @s (a >STR)' -> ARR'
    #{<>}s on whitespace characters.
     */
    case "s<>" => ssplit
    /*
    @s a (b >STR)' -> STR'
    Joins `a` with `b`.
     */
    case "><" => join
    /*
    @s a -> STR'
    #{><}s with empty string.
     */
    case "c><" => env.push(STR("")).join
    /*
    @s a -> STR'
    #{><}s with space.
     */
    case "w><" => env.push(STR(" ")).join
    /*
    @s a -> STR'
    #{><}s with newline.
     */
    case "n><" => env.push(STR("\n")).join
    /*
    @s (a >STR)' -> STR'
    Converts `STR` to `lowercase`.
     */
    case "A>a" => toLower
    /*
    @s (a >STR)' -> STR'
    Converts `STR` to `UPPERCASE`.
     */
    case "a>A" => toUpper
    /*
    @s (a >STR)' -> STR'
    Converts `STR` to `Capitalized`.
     */
    case ">Aa" => toCap
    /*
    @s (a >STR)' (r >STR)' -> SEQ[MAP]'
    Matches `a` with regex `r`.
    Each match returned is a `MAP` with the following keys:
    - ``` & ```: Matched `STR`.
    - ``` ` ```: `STR` before the match.
    - ``` ' ```: `STR` after the match.
    - ``` * ```: `ARR[MAP]` of each capturing group matched.
    - ``` ^ ```: `NUM` index of the match's start.
    - ``` $ ```: `NUM` index of the match's end.
     */
    case "/?" => rmatch
    /*
    @s (a >STR)' (r >STR)' -> SEQ[STR]'
    #{/?} with only `&` keys.
     */
    case "/?&" => rmatchMatch
    /*
    @s (a >STR)' (r >STR)' -> SEQ[STR]'
    #{/?} with only `'` keys.
     */
    case "/?`" => rmatchBefore
    /*
    @s (a >STR)' (r >STR)' -> SEQ[STR]'
    #{/?} with only ``` ` ``` keys.
     */
    case "/?'" => rmatchAfter
    /*
    @s (a >STR)' (r >STR)' -> SEQ[ARR[MAP]]'
    #{/?} with only `*` keys.
     */
    case "/?*" => rmatchGroups
    /*
    @s (a >STR)' (b >STR)' -> SEQ[NUM]'
    #{/?} with only `^` keys.
     */
    case "/?^" => rmatchStart
    /*
    @s (a >STR)' (b >STR)' -> SEQ[NUM]'
    #{/?} with only `$` keys.
     */
    case "/?$" => rmatchEnd
    /*
    @s (a >STR)' (r >STR)' (f: MAP -> >STR)' -> STR'
    Replace matches of regex `r` on `a` by applying each match `MAP` to `f`.
     */
    case "/#" => rsub
    /*
    @s (a >STR)' (r >STR)' (s >STR)' -> STR'
    Replace first match of regex `r` on `a` with `s`.
     */
    case "/#^" => rsubFirst

    /*
    @s a f' -> _'
    #{Q}s `f` on each element of `a`.
    If `a` is `MAP`, then the signature of `f` is `k v -> _`,
    where `k=>v` is the key-value pair.
    Otherwise, the signature of `f` is `x -> _`,
    where `x` is the element.
    ```sclin
    [1 2 3 4] 1.+ map
    ```
    ```sclin
    {0 1, 2 3, 4 5, } ( over + ) map
    ```
     */
    case "map" => map
    /*
    @s a f' -> a
    #{map} but `a` is preserved (i.e. leaving only side effects of `f`).
    ```sclin
    [1 2 3 4] ( 1+ n>o ) tap
    ```
     */
    case "tap" => tapMap
    /*
    @s a f' -> _'
    #{map} and #{flat}.
    ```sclin
    1224P/ \*` mapf
    ```
     */
    case "mapf" => flatMap
    /*
    @s a b (f: x y -> _)' -> _'
    #{Q}s `f` over each element-wise pair of `a` and `b`.
    Iterables of differing length truncate to the shorter length when zipped.
    ```sclin
    [1 2 3 4] [2 3 4 5] \, zip
    ```
    ```sclin
    [1 2 3 4] [2 3] \+ zip
    ```
    ```sclin
    [1 2 3 4] {1 "a", 3 "b", "x" "c", } \, zip
    ```
     */
    case "zip" => zip
    /*
    @s a b c d (f: x y -> _)' -> _'
    #{zip} but instead of truncating,
    uses `c` and `d` as fill elements for `a` and `b` respectively.
    ```sclin
    [1 2 3 4] [2 3 4 5] UN UN \, zip~
    ```
    ```sclin
    [1 2 3 4] [2 3] UN UN \+ zip~
    ```
    ```sclin
    [1 2 3 4] {1 "a", 3 "b", "x" "c", } UN UN \, zip~
    ```
     */
    case "zip~" => zip$
    /*
    @s a b (f: x y -> _)' -> _'
    #{Q}s `f` over each table-wise pair of `a` and `b`.
    ```sclin
    [1 2 3 4] [2 3 4 5] \++ tbl
    ```
     */
    case "tbl" => tbl
    /*
    @s a b (f: x y -> _)' -> _'
    #{tbl} and #{flat}.
     */
    case "tblf" => tblf
    /*
    @s a f' -> _'
    Atomic/recursive #{map}.
    ```sclin
    [[1 2] 3 4 [5 [6 7]]] ( dup n>o ) rmap
    ```
     */
    case "rmap" => rmap
    /*
    @s a b f' -> _'
    #{Q}s `f` to combine each accumulator and element starting from initial accumulator `b`.
    If `a` is `MAP`, then the signature of `f` is `k x v -> _`,
    where `k=>v` is the key-value pair and `x` is the accumulator.
    Otherwise, the signature of `f` is `x y -> _`,
    where `x` is the accumulator and `y` is the value.
    ```sclin
    [1 2 3 4] 0 \+ fold
    ```
    ```sclin
    "1011"_` =>kv 0 ( rot 2 swap ^ * + ) fold
    ```
     */
    case "fold" => fold
    /*
    @s a b f' -> _'
    #{fold} from the right.
     */
    case "foldr" => foldR
    /*
    @s a b f' -> _'
    Atomic/recursive #{fold}.
    ```sclin
    [[1 2] 3 4 [5 [6 7]]] 0 \+ rfold
    ```
    ```sclin
    [[1 2] 3 4 [5 [6 7]]] [] \+` rfold
    ```
     */
    case "rfold" => rfold
    /*
    @s a b f' -> _'
    #{rfold} from the right.
     */
    case "rfoldr" => rfoldR
    /*
    @s a f' -> _'
    #{fold} without initial accumulator, instead using the first element of `a`.
    If `a` is empty, then an error is thrown.
    ```sclin
    [1 2 3 4] \+ fold~
    ```
    ```sclin
    [1 5 10 4 3] \| fold~
    ```
     */
    case "fold~" => reduce
    /*
    @s a f' -> _'
    #{fold~} from the right.
     */
    case "foldr~" => reduceR
    /*
    @s a b f' -> _'
    #{fold} with intermediate values.
    ```sclin
    [1 2 3 4] 0 \+ scan
    ```
     */
    case "scan" => scan
    /*
    @s a b f' -> _'
    #{scan} from the right.
     */
    case "scanR" => scanR
    /*
    @s a -> NUM
    Sum of `a`. Equivalent to `0 \+ rfold`.
     */
    case "+/" => env.push(NUM(0)).push(CMD("+")).rfold
    /*
    @s a -> NUM
    Product of `a`. Equivalent to `1 \* rfold`.
     */
    case "*/" => env.push(NUM(1)).push(CMD("*")).rfold
    /*
    @s a -> _
    Minimum of `a`. Equivalent to ``` \&` fold~ ```.
     */
    case "&/" => env.push(CMD("&`")).reduce
    /*
    @s a -> _
    Maximum of `a`. Equivalent to ``` \|` fold~ ```.
     */
    case "|/" => env.push(CMD("|`")).reduce
    // TODO: this doc sucks
    /*
    @s a f' -> _'
    A multi-purpose function for creating, modifying, and traversing nested structures.
    ```sclin
    [[1 2] 3 4 { "a" 5, "b" [6 7] , }] ( dups f>o ) walk
    ```
    ```sclin
    [[1 2] 3 4 { "a" 5, "b" [6 7] , }] ( dup len 0> ( dup +` ) &# ) walk
    ```
     */
    case "walk" => walk
    /*
    @s a f' -> _'
    Keeps elements of `a` that satisfy predicate `f`.
    If `a` is `MAP`, then the signature of `f` is `k v -> >TF`,
    where `k=>v` is the key-value pair.
    Otherwise, the signature of `f` is `x -> >TF`,
    where `x` is the element.
    ```sclin
    [5 1 2 4 3] 2.> fltr
    ```
     */
    case "fltr" => fltr
    /*
    @s a f' -> TF'
    Whether any elements of `a` satisfy predicate `f`.
    See #{fltr} for the signature of `f`.
    ```sclin
    [5 1 2 4 3] 2.> any
    ```
     */
    case "any" => any
    /*
    @s a f' -> TF'
    Whether all elements of `a` satisfy predicate `f`.
    See #{fltr} for the signature of `f`.
    ```sclin
    [5 1 2 4 3] 2.> all
    ```
     */
    case "all" => all
    /*
    @s a f' -> _'
    Takes elements of `a` until #{Q}ing `f` is falsy.
    See #{fltr} for the signature of `f`.
    ```sclin
    [5 1 2 4 3] 4.!= tk*
    ```
     */
    case "tk*" => tkwl
    /*
    @s a f' -> _'
    Drops elements of `a` while predicate `f` is truthy.
    See #{fltr} for the signature of `f`.
    ```sclin
    [5 1 2 4 3] 4.!= dp*
    ```
     */
    case "dp*" => dpwl
    /*
    @s a f' -> _'
    Finds first element of `a` where predicate `f` is truthy.
    See #{fltr} for the signature of `f`.
    Returns `UN` if not found.
    ```sclin
    [5 1 2 4 3] ( 2% ! ) find
    ```
     */
    case "find" => find
    /*
    @s a f' -> NUM'
    Finds index of first element of `a` where predicate `f` is truthy.
    See #{fltr} for the signature of `f`.
    Returns `-1` if not found.
    ```sclin
    [5 1 2 4 3] ( 2% ! ) find:
    ```
     */
    case "find:" => findi
    /*
    @s a f' -> _'
    Deletes first element of `a` where predicate `f` is truthy.
    See #{fltr} for the signature of `f`.
    ```sclin
    [5 1 2 4 3] ( 2% ! ) del
    ```
     */
    case "del" => del
    /*
    @s a f' -> _'
    Uniquifies elements of `a` with mapper `f`.
    See #{map} for the signature of `f`.
    ```sclin
    [2 4 3 3 5 4 1] () uniq
    ```
    ```sclin
    [5 1 2 4 3] 2.% uniq
    ```
     */
    case "uniq" => uniq
    /*
    @s a f' -> _'
    Uniquifies elements of `a` with comparator `f`.
    See #{sort~} for the signature of `f`.
    ```sclin
    [2 4 3 3 5 4 1] \=` uniq~
    ```
    ```sclin
    [2 4 3 3 5 4 1] 2.% uniq~
    ```
     */
    case "uniq~" => uniq$
    /*
    @s a f' -> _'
    Sorts elements of `a` with mapper `f`.
    See #{map} for the signature of `f`.
    ```sclin
    ["a" "" "abc" "ab"] \len sort
    ```
    ```sclin
    [1 2 3 4 5] \$rng sort
    ```
     */
    case "sort" => sort
    /*
    @s a f' -> _'
    Sorts elements of `a` with comparator `f`.
    If `a` is `MAP`, then the signature of `f` is `ARR[k v] ARR[j w] -> >TF`,
    where `k=>v` and `j=>w` are key-value pairs to compare.
    Otherwise, the signature of `f` is `x y -> >TF`,
    where `x` and `y` are elements to compare.
    ```sclin
    [1 5 2 3 4] \< sort~
    ```
    ```sclin
    [1 5 2 3 4] \> sort~
    ```
     */
    case "sort~" => sort$
    /*
    @s a f' -> ARR[_ _]'
    Separates `a` into 2 parts based on predicate `f`.
    See #{fltr} for the signature of `f`.
    ```sclin
    [5 1 2 4 3] 2.> part
    ```
     */
    case "part" => part
    /*
    @s a f' -> MAP'
    Separates `a` groups based on `f`.
    Each result of `f` becomes a key in the resulting `MAP`.
    See #{map} for the signature of `f`.
    ```sclin
    "abc"^set >A \len group
    ```
     */
    case "group" => group
    /*
    @s a f' -> ARR[_ _]'
    Equivalent to a combination of #{tk*} and #{dp*}.
    See #{fltr} for the signature of `f`.
    ```sclin
    [5 1 2 4 3] 2.% span
    ```
     */
    case "span" => span
    /*
    @s a f' -> _'
    Groups consecutive duplicate runs of `a` based on predicate `f`.
    See #{sort~} for the signature of `f`.
    ```sclin
    [1 1 2 3 3 4 6 4 4] \=` pack
    ```
     */
    case "pack" => pack

    /*
    @s a b f' -> _'
    Gets the union of `a` and `b` with comparator `f`.
    See #{sort~} for the signature of `f`.
    ```sclin
    [1 2 3 4] [2 4 6 8] \=` union
    ```
     */
    case "union" => union
    /*
    @s a b f' -> _'
    Gets the intersection between `a` and `b` with comparator `f`.
    May hang if `a` or `b` are infinite.
    See #{sort~} for the signature of `f`.
    ```sclin
    [1 2 3 4] [2 4 6 8] \=` intxn
    ```
     */
    case "intxn" => intersect
    /*
    @s a b f' -> _'
    Gets the difference between `a` and `b` with comparator `f`.
    Will hang if `b` is infinite.
    See #{sort~} for the signature of `f`.
    ```sclin
    [1 2 3 4] [2 4 6 8] \=` diff
    ```
     */
    case "diff" => diff

    /*
    @s (a >FUT[x])' -> x'
    Synchronously waits for `a` to complete, leaving the result on the stack.
     */
    case "~_" => await
    /*
    @s (a >FUT[x])' -> TRY[x]'
    #{~_} with result wrapped in a `TRY`.
     */
    case "~_!" => awaitTRY
    /*
    @s (a >FUT)' ->
    Cancels `a`.
     */
    case "~$" => cancelFUT
    /*
    @s a[>TASK*] -> TASK[_[_*]]
    Executes each `TASK` in `a` sequentially such that both effects and results are ordered.
     */
    case "~|>" => seqTASK
    /*
    @s a[>TASK*] -> TASK[_[_*]]
    Executes each `TASK` in `a` in parallel such that effects are unordered but results are ordered.
     */
    case "~||" => parTASK
    /*
    @s a[>TASK*] (n >NUM) -> TASK[_[_*]]
    #{~||} but with at most `n` concurrently running `TASK`s.
     */
    case "~||>" => parTASK
    /*
    @s a[>TASK*] -> TASK[_[_*]]
    #{~||} but results are also unordered.
     */
    case "~//" => parunTASK
    /*
    @s a[>TASK*] -> TASK
    Races a collection of `TASK`s, returning the first to complete.
     */
    case "~>>" => raceTASK
    /*
    @s (a >TASK)' -> TASK'
    Ensures that `a` runs on a separate thread.
     */
    case "~<" => forkTASK
    /*
    @s (a >TASK)' -> TASK'
    Ensures that `a` is memoized such that subsequent runs of the task return the same value.
     */
    case "~:" => memoTASK
    /*
    @s (a >TASK)' -> TASK'
    #{~:} but only if `a` completes successfully.
     */
    case "~:&" => memoTASK$
    /*
    @s (a >TASK)' -> TASK'
    Ensures that `a` is uncancellable.
     */
    case "~$_" => uncancelTASK
    /*
    @s (a >TASK)' (n >NUM)' -> TASK'
    Ensures that `a` will error if not completed within `n` milliseconds.
     */
    case "~%" => timeoutTASK
    /*
    @s (n >NUM)' -> TASK[n]'
    Creates an asynchronous `TASK` that will complete after `n` milliseconds.
     */
    case "sleep" => sleep

    case _ => throw LinEx("FN", s"unknown fn \"$x\"")

    // CMDOC END

  def eval: ENV = env.arg1((x, env) =>
    x match
      case f: FN =>
        val env1 =
          env.copy(code = f)
        env.code.x match
          case LazyList() => env1
          case _          => env.modStack(_ => env1.addCall(f).exec.stack)
      case _ => env.push(x).toFN.eval
  )
  def evale: ENV = env.arg1((x, env) =>
    x match
      case f: FN => env.modStack(_ => env.copy(code = f).addCall(f).exec.stack)
      case _     => env.push(x).toFN.evale
  )
  def evalS(x: ARRW[ANY], f: ANY): ARRW[ANY] =
    env.modStack(_ => x :+ f).evale.stack
  def evalA1(x: ARRW[ANY], f: ANY): ANY =
    env.modStack(_ => x :+ f).evale.getStack(0)
  def evalA2(x: ARRW[ANY], f: ANY): (ANY, ANY) =
    val env1 = env.modStack(_ => x :+ f).evale
    (env1.getStack(1), env1.getStack(0))
  def quar: ENV =
    env.arg1((x, env) => x.vec1(env.push(_).evale.getStack(0)).pipe(env.push))

  def startFN: ENV =
    val l = Lambda(env.code.x).loop
    val (cs, c) = l.ys match
      case cs :+ c => (cs, c.toString)
      case _       => (l.ys, ")")
    env.modCode(_ => l.xs).push(FN(env.code.p, cs)).cmd(c)

  def evalLine: ENV = env.arg1((x, env) =>
    val i    = x.toInt
    val env1 = env.fnLine(i)
    env1.push(env1.getLineF(i)).eval
  )
  def getLNum: ENV = env.push(NUM(env.code.p.l))
  def getLFile: ENV = env.push(env.code.p.f match
    case Some(x) => STR(x.toString)
    case _       => UN
  )
  def getLns: ENV    = env.push(env.lines.map { case (_, (l, _)) => l }.toARR)
  def evalLRel: ENV  = getLNum.add.evalLine
  def evalLHere: ENV = env.push(NUM(0)).evalLRel
  def evalLNext: ENV = env.push(NUM(1)).evalLRel
  def evalLPrev: ENV = env.push(NUM(-1)).evalLRel
  def getLn: ENV     = env.mod1(n => env.getLineS(n.toNUM.x.intValue))
  def getLRel: ENV   = getLNum.add.getLn
  def getLHere: ENV  = env.push(NUM(0)).getLRel
  def getLNext: ENV  = env.push(NUM(1)).getLRel
  def getLPrev: ENV  = env.push(NUM(-1)).getLRel
  def evalAnd: ENV =
    env.arg2((x, f, env) => if x.toBool then env.push(f).eval else env)
  def evalOr: ENV =
    env.arg2((x, f, env) => if x.toBool then env else env.push(f).eval)
  def evalIf: ENV =
    env.arg3((x, f, g, env) => env.push(if x.toBool then f else g).eval)
  def evalIf$ : ENV =
    env.arg1((f, env) =>
      f.toMAP.x.find { case (k, _) =>
        env.push(k).evale.getStack(0).toBool
      } match
        case Some(_, v) => env.push(v).eval
        case _          => env
    )
  def evalTimes: ENV =
    env.arg2((f, n, env) =>
      def loop(env: ENV, n: NUMF): ENV =
        if n.compare(0) > 0 then loop(env.push(f).evale, n - 1)
        else env
      loop(env, n.toNUM.x)
    )
  def evalTry: ENV = env.arg2((f, g, env) =>
    try env.push(f).evale
    catch case e => env.pushs(Vector(e.toERRW(env), g)).quar.pop
  )
  def evalTRY: ENV = env.arg1((x, env) =>
    env.push(x.vec1(f => Try(env.push(f).quar.getStack(0)).toTRY))
  )
  def throwERR: ENV = env.arg1((x, env) => throw x.toThrow)
  def evalArrSt: ENV = env.arg2((x, f, env) =>
    env.push(env.push(x).unwrap$.push(f).evale.stack.toARR.matchType(x))
  )
  def evalStArr: ENV = env.arg1((f, env) => env.wrap$$.push(f).quar.unwrap$)

  def startARR: ENV = env.setArr(env.stack :: env.arr).clr
  def endARR: ENV = env.arr match
    case List()  => env
    case x :: xs => env.setArr(xs).modStack(_ => x).push(env.stack.toARR)
  def endMAP: ENV = endARR.toMAP

  def getType: ENV = env.mod1(_.getType.sSTR)
  def toSEQ: ENV   = env.mod1(_.toSEQ)
  def toARR: ENV   = env.mod1(_.toARR)
  def toMAP: ENV   = env.mod1(_.toMAP)
  def toSTR: ENV   = env.mod1(_.toSTR)
  def vSTR: ENV    = env.vec1(_.toSTR)
  def toNUM: ENV   = env.mod1(_.toNUM)
  def vNUM: ENV    = env.vec1(_.toNUM)
  def toDBL: ENV   = env.mod1(_.toDBL)
  def vDBL: ENV    = env.vec1(_.toDBL)
  def toFN: ENV    = env.mod1(_.toFN(env))
  def toTASK: ENV  = env.mod1(_.toTASK)
  def toTRY: ENV   = env.mod1(_.toTRY)
  def toERR: ENV   = env.mod2((x, y) => ERR(LinERR(env, y.toString, x.toString)))
  def toTF: ENV    = env.mod1(_.toTF)
  def toNUMD: ENV =
    env.mod2((x, y) => y.vec1(_.toInt.pipe(x.toNUM.x.getString).sSTR))
  def matchType: ENV = env.mod2(_.matchType(_))
  def lineMAP: ENV = env.vec1(
    _.toSTR.x
      .split("\n")
      .map(s => env.evalA2(Vector(), STR(s)))
      .to(VectorMap)
      .toMAP
  )
  def fromJSON: ENV = env.vec1(_.toString.pipe(read(_)))
  def toJSON: ENV   = env.mod1(write(_).sSTR)

  def locId: ENV = env.arg1((x, env) =>
    x.vef1(env)((env, cs) =>
      cs.xFN.foldLeft(env)((env, c) => env.addLocId(c.toString))
    )
  )
  def globId: ENV = env.arg1((x, env) =>
    x.vef1(env)((env, cs) =>
      cs.xFN.foldLeft(env)((env, c) => env.addGlobId(c.toString))
    )
  )

  def lambda: ENV = env.arg1((x, env) =>
    val x1 = x.xFN
    env.arg(
      x1.length,
      (cs, env) =>
        x1.lazyZip(cs).foldLeft(env) { case (env, (k, v)) =>
          env.addLoc(k.toString, v)
        }
    )
  )

  def getSc =
    env.push(MAP(env.scope.map { case (k, v) => (k.sSTR, v) }.to(VectorMap)))

  def in: ENV   = env.push(STR(readLine))
  def out: ENV  = env.arg1((x, env) => env.tap(_ => print(x)))
  def outn: ENV = env.arg1((x, env) => env.tap(_ => println(x)))

  def form: ENV = env.mod1(_.toForm.sSTR)
  def outf: ENV = env.form.outn

  def dup: ENV  = env.mods1(x => Vector(x, x))
  def dups: ENV = env.push(env.stack.toARR)
  def dupd: ENV = env.mods2((x, y) => Vector(x, x, y))
  def over: ENV = env.mods2((x, y) => Vector(x, y, x))
  def ddup: ENV = env.mods2((x, y) => Vector(x, y, x, y))
  def edup: ENV = env.mods3((x, y, z) => Vector(x, y, z, x, y, z))
  def pick: ENV =
    env.arg1((x, env) => env.push(x.vec1(n => env.getStack(n.toInt))))

  def pop: ENV = env.mods1(_ => Vector())
  def clr: ENV = env.modStack(_ => Vector())
  def nip: ENV = env.mod2((_, x) => x)
  def nix: ENV = env.arg1((x, env) =>
    env.modStack(s =>
      val i = env.iStack(x.toInt)
      if 0 <= i && i < s.length then s.patch(i, Nil, 1) else s
    )
  )

  def swap: ENV  = env.mods2((x, y) => Vector(y, x))
  def rev: ENV   = env.modStack(_.reverse)
  def swapd: ENV = env.mods3((x, y, z) => Vector(y, x, z))
  def tuck: ENV  = env.mods2((x, y) => Vector(y, x, y))
  def trade: ENV =
    env.arg1((x, env) => env.push(x).rollu.push(x).push(NUM(1)).sub.roll)

  def rot: ENV  = env.mods3((x, y, z) => Vector(y, z, x))
  def rotu: ENV = env.mods3((x, y, z) => Vector(z, x, y))
  def roll: ENV = env.arg1((x, env) =>
    val a = env.getStack(x.toInt)
    env.push(x).nix.push(a)
  )
  def rollu: ENV = env.arg1((x, env) =>
    val a = env.getStack(0)
    env.modStack(s => s.patch(env.iStack(x.toInt), Vector(a), 0)).pop
  )

  def dip: ENV = env.arg2((x, f, env) => env.push(f).evale.push(x))

  def get: ENV    = env.mod2((x, y) => y.vec1(x.get(_)))
  def get$$ : ENV = env.mod2(_.get(_))
  def gets: ENV   = env.mod2(_.gets(_))

  def set: ENV  = env.mod3((x, y, i) => x.set(i, y))
  def sets: ENV = env.mod3((x, y, i) => x.sets(i.toSEQ.x, y))
  def setmod: ENV =
    env.mod3((x, f, i) => x.setmod(i, a => env.evalA1(Vector(a), f)))
  def setmods: ENV =
    env.mod3((x, f, i) => x.setmods(i.toSEQ.x, a => env.evalA1(Vector(a), f)))

  def idel: ENV = env.mod2(_.remove(_))

  def has: ENV    = env.mod2((x, y) => y.vec1(x.has(_).boolTF))
  def has$$ : ENV = env.mod2(_.has(_).boolTF)

  def len: ENV = env.mod1(_.length.pipe(NUM(_)))

  def rep: ENV  = env.mod1(LazyList.continually(_).toSEQ)
  def cyc: ENV  = env.mod1(LazyList.continually(_).toSEQ.flat)
  def ones: ENV = env.vec1(_.toInt.pipe(Vector.fill(_)(NUM(1)).toARR))
  def one$ : ENV =
    env.mod1(_.foldRight(NUM(1))((x, y) => Vector.fill(x.toInt)(y).toARR))
  def toShape: ENV =
    env.mod2(_.toShape(_))

  def itr: ENV = env.mod2((x, y) =>
    y.vec1(f => LazyList.iterate(x)(s => env.evalA1(Vector(s), f)).toSEQ)
  )
  def unfold: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      LazyList
        .unfold(x)(s =>
          env.evalS(Vector(s), f) match
            case st :+ n =>
              st match
                case _ :+ m => Some(m, n)
                case _      => throw LinEx("ST_LEN", "stack length = 1")
            case _ => None
        )
        .toSEQ
    )
  )

  def enumL: ENV = env.mod1 {
    case x: MAP => x.toARR
    case x =>
      LazyList
        .from(0)
        .map(NUM(_))
        .toSEQ
        .zip(x, Vector(_, _).toARR)
        .matchType(x)
  }
  def keys: ENV = env.enumL.mod1(_.map(_.get(NUM(0))))
  def vals: ENV = env.enumL.mod1(_.map(_.get(NUM(1))))

  def rhelper(r: (Real, Real) => Range): ENV =
    env.num2a(r(_, _).iterator.map(Real(_)).toVector)
  def range: ENV =
    rhelper((x, y) => x.intValue until y.intValue by (y > x).boolInt * 2 - 1)
  def irange: ENV =
    rhelper((x, y) => x.intValue to y.intValue by (y > x).boolInt * 2 - 1)

  def shuffle: ENV = env.mod1(_.shuffle)
  def getr: ENV    = env.shuffle.push(NUM(0)).get
  def perm: ENV    = env.mod1(_.permutations)
  def comb: ENV    = env.mod2((x, y) => y.vec1(n => x.combinations(n.toInt)))
  def powset: ENV  = env.mod1(_.powset)
  def cProd: ENV = env.mod1(x =>
    x.toSEQ.x
      .map(_.toSEQ.x)
      .pipe(ANY.cProd)
      .map(_.toSEQ.matchType(x.get(NUM(0))))
      .toSEQ
  )
  def tpose: ENV = env.mod1(x =>
    def f(a: ANY): LazyList[ANY] = a match
      case MAP(a) => a.values.to(LazyList)
      case _      => a.toSEQ.x
    def g(a: LazyList[ANY], b: ANY): ANY = b match
      case _: SEQ => a.toSEQ
      case _: MAP => a.toARR
      case Its(_) => a.toARR.matchType(b)
      case _      => a.toARR
    val x1 = f(x)
    x1.map(f)
      .pipe(ANY.transpose)
      .map(g(_, x))
      .pipe(g(_, x1.headOption.getOrElse(UN.toARR)))
  )

  def padH(f: (String, Int, String) => String): ENV = env.vec3((x, y, z) =>
    val x1 = x.toString
    val y1 = y.toInt - x1.length
    val z1 = z.toString
    f(x1, y1, z1).sSTR
  )
  def padH1(s: String, n: Int): String =
    LazyList.continually(s).flatten.take(n).mkString
  def pad: ENV  = padH((x, y, z) => x + padH1(z, y))
  def padl: ENV = padH((x, y, z) => padH1(z, y) + x)
  def padc: ENV = padH((x, y, z) =>
    val q  = y / 2
    val y1 = y - q
    padH1(z, q) + x.toString + padH1(z, y1)
  )

  def pad$H(f: (ANY, Int, ANY) => ANY): ENV = env.mod3((x, y, z) =>
    y.vec1(y =>
      val y1 = y.toInt - x.length
      if x.toSEQ.x.drop(y1).isEmpty then f(x, y1, z) else x
    )
  )
  def pad$H1(s: ANY, n: Int): ANY = ARR(Vector()).add$$(s).mul$$(NUM(n))
  def pad$ : ENV                  = pad$H((x, y, z) => x.add$$(pad$H1(z, y)))
  def padl$ : ENV                 = pad$H((x, y, z) => pad$H1(z, y).add$$(x))
  def padc$ : ENV = pad$H((x, y, z) =>
    val q  = y / 2
    val y1 = y - q
    pad$H1(z, q).add$$(x).add$$(pad$H1(z, y1))
  )

  def toCodePt: ENV =
    env.mod1(_.vec1(x => x.toString.map(_.toInt.pipe(NUM(_))).toARR))
  def fromCodePt: ENV = env.mod1(
    _.map(_.toInt.toChar.toString.sSTR).toString.sSTR
  )

  def split: ENV   = env.str2a(_.split(_))
  def ssplit: ENV  = env.str1a(_.split(raw"\s"))
  def join: ENV    = env.mod2((x, y) => y.str1(x.join))
  def toUpper: ENV = env.str1(_.toUpperCase)
  def toLower: ENV = env.str1(_.toLowerCase)
  def toCap: ENV   = env.str1(_.capitalize)

  def rmatchBase(x: ANY, r: ANY): Iterator[MAP] =
    r.toSTR.x.r.findAllMatchIn(x.toSTR.x).map(_.matchMAP)
  def rmatch: ENV       = env.vec2(rmatchBase(_, _).toSEQ)
  def rmatchMatch: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("&"))).toSEQ)
  def rmatchBefore: ENV = env.vec2(rmatchBase(_, _).map(_.get(STR("`"))).toSEQ)
  def rmatchAfter: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("'"))).toSEQ)
  def rmatchGroups: ENV = env.vec2(rmatchBase(_, _).map(_.get(STR("*"))).toSEQ)
  def rmatchStart: ENV  = env.vec2(rmatchBase(_, _).map(_.get(STR("^"))).toSEQ)
  def rmatchEnd: ENV    = env.vec2(rmatchBase(_, _).map(_.get(STR("$"))).toSEQ)
  def rsub: ENV = env.vec3((x, r, f) =>
    r.toSTR.x.r
      .replaceAllIn(x.toSTR.x, m => env.evalA1(Vector(m.matchMAP), f).toSTR.x)
      .sSTR
  )
  def rsubFirst: ENV =
    env.vec3((x, r, f) => r.toSTR.x.r.replaceFirstIn(x.toSTR.x, f.toSTR.x).sSTR)

  def wrap$ : ENV   = env.modx(2, _.toARR)
  def wrap: ENV     = env.modx(1, _.toARR)
  def wrap$$ : ENV  = env.modStack(x => Vector(x.toARR))
  def wrapv$ : ENV  = env.vec2(Vector(_, _).toARR)
  def wrapv: ENV    = env.vec1(Vector(_).toARR)
  def unwrap: ENV   = env.mods1(_.toARR.x)
  def unwrap$ : ENV = env.arg1((x, env) => env.modStack(_ => x.toARR.x))
  def wrapFN: ENV   = env.wrap.mod1(_.toFN(env))

  def tk: ENV = env.mod2((x, y) => y.vec1(n => x.take(n.toInt)))
  def dp: ENV = env.mod2((x, y) => y.vec1(n => x.drop(n.toInt)))
  def splitAt: ENV = env.mod2((x, y) =>
    y.vec1(n =>
      val i = n.toInt
      Vector(x.take(i), x.drop(i)).toARR
    )
  )

  def scale: ENV = env.push(NUM(10)).swap.pow.mul
  def trunc: ENV = env.num1(_.toBigInt.toDouble, _.toBigInt)
  def floor: ENV = env.num1(_.floor, _.floor)
  def round: ENV = env.num1(_.round, _.round)
  def ceil: ENV  = env.num1(_.ceil, _.ceil)

  def fromDec: ENV =
    env.num2a((x, y) => ANY.fromDec(x.toSafeLong, y.toInt).map(Real(_)))
  def toDec: ENV = env.mod2((x, y) =>
    val x1 = x.toARR.x.map(_.toNUM.x.toSafeLong)
    y.num1(
      n => ANY.toDec(x1, n.toInt).toDouble,
      n => ANY.toDec(x1, n.toInt)
    )
  )
  def toNumDen: ENV = env.vec1(x =>
    val a = x.toNUM.x.toRational
    Vector(a.numerator, a.denominator).map(NUM(_)).toARR
  )
  def isInt: ENV = env.vec1(_.toNUM.x.isWhole.boolTF)
  def isExact: ENV = env.vec1(x =>
    NUM(x.toNUM.x match
      case Real.Exact(_) => 1
      case _             => 0
    )
  )

  def neg: ENV   = env.num1(-_, -_)
  def neg$ : ENV = env.str1(_.reverse)
  def neg$$ : ENV =
    def loop(t: ANY): ANY = t match
      case Lsy(x) => x.reverse.toSEQ.matchType(t)
      case ARR(x) => x.reverse.toARR
      case _: MAP => loop(t.toSEQ).toMAP
      case _      => t.str1(_.reverse)
    env.mod1(loop)

  def add: ENV    = env.num2(_ + _, _ + _)
  def add$ : ENV  = env.str2(_ ++ _)
  def add$$ : ENV = env.mod2(_ add$$ _)
  def cons: ENV   = env.mod2(_ cons _)
  def snoc: ENV   = env.mod2(_ snoc _)
  def uncons: ENV = env.mods1(x => Vector(x.get(NUM(0)), x.drop(1)))
  def unsnoc: ENV = env.mods1(x => Vector(x.get(NUM(-1)), x.drop(-1)))

  def sub: ENV    = env.num2(_ - _, _ - _)
  def sub$ : ENV  = env.str2((x, y) => x.filterNot(y.contains))
  def sub$$ : ENV = env.mod2(_ sub$$ _)

  def mul: ENV    = env.num2(_ * _, _ * _)
  def mul$ : ENV  = env.strnum(_ * _.intValue)
  def mul$$ : ENV = env.mod2(_ mul$$ _)

  def div: ENV = env.num2(
    _ / _,
    (x, y) => if y == 0 then throw ArithmeticException() else x / y,
    "bad /"
  )
  def divi: ENV = env.num2(
    _.fquot(_),
    (x, y) => if y == 0 then throw ArithmeticException() else x.fquot(y),
    "bad /~"
  )
  def div$ : ENV  = env.strnumq((x, y) => x.grouped(y.intValue).to(LazyList))
  def div$$ : ENV = env.mod2((x, y) => y.vec1(x div$$ _))

  def mod: ENV = env.num2(
    _.fmod(_),
    (x, y) => if y == 0 then throw ArithmeticException() else x.fmod(y),
    "bad %"
  )
  def divmod: ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).divi.pushs(Vector(x, y)).mod
  )
  def mod$ : ENV  = env.strnumq((x, y) => x.sliding(y.intValue).to(LazyList))
  def mod$$ : ENV = env.mod2((x, y) => y.vec1(x mod$$ _))

  def pow: ENV = env.num2(
    _.fpow(_),
    (x, y) =>
      if x < 0 && y.reciprocal.fmod(2) == 0 then throw ArithmeticException()
      else x.fpow(y),
    "bad ^"
  )
  def powi: ENV = env.num2(_ ** _.intValue, _ ** _.intValue)
  def pow$ : ENV = env.vec2((x, y) =>
    ANY.cPow(x.toSTR.toSEQ.x, y.toInt).map(_.toARR.toSTR).toSEQ
  )
  def pow$$ : ENV = env.mod2((x, y) =>
    y.vec1(n => ANY.cPow(x.toSEQ.x, n.toInt).map(_.toARR.matchType(x)).toSEQ)
  )

  def exp: ENV = env.num1(_.exp, _.exp)
  def abs: ENV = env.num1(_.abs, _.abs)

  def sin: ENV = env.num1(math.sin, Real.sin)
  def cos: ENV = env.num1(math.cos, Real.cos)
  def tan: ENV = env.num1(
    math.tan,
    x =>
      val d = Real.cos(x)
      if d == 0 then throw ArithmeticException() else Real.sin(x) / d
    ,
    "bad tan"
  )
  def asin: ENV  = env.num1(math.asin, Real.asin)
  def acos: ENV  = env.num1(math.acos, Real.acos)
  def atan: ENV  = env.num1(math.atan, Real.atan)
  def atan2: ENV = env.num2(math.atan2, Real.atan2)
  def sinh: ENV  = env.num1(math.sinh, Real.sinh)
  def cosh: ENV  = env.num1(math.cosh, Real.cosh)
  def tanh: ENV  = env.num1(math.tanh, Real.tanh)
  def asinh: ENV = env.num1(Util.asinh, Real.asinh)
  def acosh: ENV = env.num1(Util.acosh, Real.acosh)
  def atanh: ENV = env.num1(Util.atanh, Real.atanh)

  def ln: ENV =
    env.num1(
      math.log,
      x => if x <= 0 then throw ArithmeticException() else Real.log(x),
      "bad log"
    )
  def log: ENV   = env.arg2((x, y, env) => env.push(x).ln.push(y).ln.div)
  def log10: ENV = env.push(NUM(10)).log

  def isPrime: ENV = env.vec1(_.toNUM.x.toSafeLong.pipe(prime.isPrime).boolTF)
  def factor: ENV = env.vec1(
    _.toNUM.x.toSafeLong
      .pipe(prime.factor)
      .to(VectorMap)
      .map { case (x, y) => (NUM(x), NUM(y)) }
      .toMAP
      .sortByM((a, _) => a, a => a)
  )

  def not: ENV    = env.vec1(_.toBool.unary_!.boolTF)
  def not$$ : ENV = env.mod1(_.toBool.unary_!.boolTF)
  def min: ENV    = env.vec2((x, y) => if x.cmp(y) < 0 then x else y)
  def and: ENV    = env.vec2((x, y) => (x.toBool && y.toBool).boolTF)
  def min$$ : ENV = env.mod2((x, y) => if x.cmp(y) < 0 then x else y)
  def and$$ : ENV = env.mod2((x, y) => (x.toBool && y.toBool).boolTF)
  def max: ENV    = env.vec2((x, y) => if x.cmp(y) > 0 then x else y)
  def or: ENV     = env.vec2((x, y) => (x.toBool || y.toBool).boolTF)
  def max$$ : ENV = env.mod2((x, y) => if x.cmp(y) > 0 then x else y)
  def or$$ : ENV  = env.mod2((x, y) => (x.toBool || y.toBool).boolTF)

  def cmp: ENV    = env.vec2((x, y) => NUM(x.cmp(y)))
  def cmp$$ : ENV = env.mod2((x, y) => NUM(x.cmp(y)))
  def lt: ENV     = cmp.push(NUM(-1)).eql
  def lt$$ : ENV  = cmp$$.push(NUM(-1)).eql
  def gt: ENV     = cmp.push(NUM(1)).eql
  def gt$$ : ENV  = cmp$$.push(NUM(1)).eql
  def lteq: ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).lt.pushs(Vector(x, y)).eql.or
  )
  def lteq$$ : ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).lt$$.pushs(Vector(x, y)).eql$$.or
  )
  def gteq: ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).gt.pushs(Vector(x, y)).eql.or
  )
  def gteq$$ : ENV = env.arg2((x, y, env) =>
    env.pushs(Vector(x, y)).gt$$.pushs(Vector(x, y)).eql$$.or
  )
  def eql: ENV    = env.vec2(_.eql(_).boolTF)
  def eql$$ : ENV = env.mod2(_.eql(_).boolTF)
  def neq: ENV    = eql.not
  def neq$$ : ENV = eql$$.not

  def SIG_1f1(f: ANY)(a: ANY): ANY     = env.evalA1(Vector(a), f)
  def SIG_1f_(f: ANY)(a: ANY): ANY     = SIG_1f1(f)(a).pipe(_ => a)
  def SIG_1fb(f: ANY)(a: ANY): Boolean = SIG_1f1(f)(a).toBool

  def SIG_2f1(f: ANY)(a: ANY, b: ANY): ANY = env.evalA1(Vector(a, b), f)
  def SIG_2f_(f: ANY)(a: ANY, b: ANY): (ANY, ANY) =
    SIG_2f1(f)(a, b).pipe(_ => (a, b))
  def SIG_2fb(f: ANY)(a: ANY, b: ANY): Boolean    = SIG_2f1(f)(a, b).toBool
  def SIG_2f2(f: ANY)(a: ANY, b: ANY): (ANY, ANY) = env.evalA2(Vector(a, b), f)

  def SIG_1y2f1(f: ANY)(a: ANY, b: (ANY, ANY)): ANY =
    env.evalA1(Vector(b._1, a, b._2), f)
  def SIG_2y1f1(f: ANY)(a: (ANY, ANY), b: ANY): ANY =
    env.evalA1(Vector(a._1, a._2, b), f)
  def SIG_2x2fb(f: ANY)(a: (ANY, ANY), b: (ANY, ANY)): Boolean =
    env
      .evalA1(Vector(Vector(a._1, a._2).toARR, Vector(b._1, b._2).toARR), f)
      .toBool

  def map: ENV =
    env.mod2((x, y) => y.vec1(f => x.mapM(SIG_2f2(f), SIG_1f1(f))))
  def tapMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.mapM(SIG_2f_(f), SIG_1f_(f))))
  def flatMap: ENV =
    env.mod2((x, y) => y.vec1(f => x.flatMapM(SIG_2f1(f), SIG_1f1(f))))
  def flat: ENV  = env.mod1(_.flat)
  def rflat: ENV = env.mod1(_.rflat)
  def rmap: ENV  = env.mod2((x, y) => y.vec1(f => x.rmap(SIG_1f1(f))))

  def zip: ENV = env.mod3((x, y, z) => z.vec1(f => x.zip(y, SIG_2f1(f))))
  def zip$ : ENV = env.modx(
    5,
    {
      case Vector(x, y, v, w, z) => z.vec1(f => x.zipAll(y, v, w, SIG_2f1(f)))
      case _                     => ???
    }
  )
  def tbl: ENV  = env.mod3((x, y, z) => z.vec1(f => x.table(y, SIG_2f1(f))))
  def tblf: ENV = env.mod3((x, y, z) => z.vec1(f => x.flatTable(y, SIG_2f1(f))))

  def fold: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.foldLeftM(y)(SIG_1y2f1(f), SIG_2f1(f))))
  def foldR: ENV =
    env.mod3((x, y, z) =>
      z.vec1(f => x.foldRightM(y)(SIG_2y1f1(f), SIG_2f1(f)))
    )
  def rfold: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.rfoldLeft(y)(SIG_2f1(f))))
  def rfoldR: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.rfoldRight(y)(SIG_2f1(f))))
  def reduce: ENV =
    env.mod2((x, y) => y.vec1(f => x.reduceLeftM(SIG_1y2f1(f), SIG_2f1(f))))
  def reduceR: ENV =
    env.mod2((x, y) => y.vec1(f => x.reduceRightM(SIG_1y2f1(f), SIG_2f1(f))))
  def scan: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.scanLeftM(y)(SIG_1y2f1(f), SIG_2f1(f))))
  def scanR: ENV =
    env.mod3((x, y, z) =>
      z.vec1(f => x.scanRightM(y)(SIG_2y1f1(f), SIG_2f1(f)))
    )

  def walk: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      def loop(b: Boolean = false)(xs: ARRW[ANY]): ARRW[ANY] = xs match
        case st :+ n =>
          st match
            case st :+ m if b =>
              st match
                case _ :+ k => Vector(k, if n.toBool then fn(m) else m)
                case _ =>
                  val n1 = n match
                    case Itr(_) => fn(n)
                    case _      => n
                  Vector(m, n1)
            case _ :+ m => Vector(if n.toBool then fn(m) else m)
            case _ =>
              Vector(n match
                case Itr(_) => fn(n)
                case _      => n
              )
        case _ => Vector()
      def fn(t: ANY): ANY =
        t.flatMap$M(
          (k, v) => env.evalS(Vector(k, v), f).pipe(loop(true)),
          v => env.evalS(Vector(v), f).pipe(loop())
        )
      env.evalS(Vector(x), f).pipe(loop()).lastOption.getOrElse(UN)
    )
  )

  def fltr: ENV =
    env.mod2((x, y) => y.vec1(f => x.filterM(SIG_2fb(f), SIG_1fb(f))))

  def any: ENV =
    env.mod2((x, y) => y.vec1(f => x.anyM(SIG_2fb(f), SIG_1fb(f)).boolTF))
  def all: ENV =
    env.mod2((x, y) => y.vec1(f => x.allM(SIG_2fb(f), SIG_1fb(f)).boolTF))

  def tkwl: ENV =
    env.mod2((x, y) => y.vec1(f => x.takeWhileM(SIG_2fb(f), SIG_1fb(f))))
  def dpwl: ENV =
    env.mod2((x, y) => y.vec1(f => x.dropWhileM(SIG_2fb(f), SIG_1fb(f))))

  def find: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      x.findM(SIG_2fb(f), SIG_1fb(f)) match
        case Some((a, b)) => Vector(a, b).toARR
        case Some(a: ANY) => a
        case _            => UN
    )
  )

  def findi: ENV =
    env.mod2((x, y) => y.vec1(f => x.findIndex(SIG_1fb(f)).pipe(NUM(_))))

  def del: ENV =
    env.mod2((x, y) => y.vec1(f => x.delByM(SIG_2fb(f), SIG_1fb(f))))

  def uniq: ENV =
    env.mod2((x, y) => y.vec1(f => x.uniqByM(SIG_2f1(f), SIG_1f1(f))))
  def uniq$ : ENV =
    env.mod2((x, y) => y.vec1(f => x.uniqWithM(SIG_2x2fb(f), SIG_2fb(f))))

  def sort: ENV =
    env.mod2((x, y) => y.vec1(f => x.sortByM(SIG_2f1(f), SIG_1f1(f))))
  def sort$ : ENV =
    env.mod2((x, y) => y.vec1(f => x.sortWithM(SIG_2x2fb(f), SIG_2fb(f))))

  def part: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      val (a, b) = x.partitionM(SIG_2fb(f), SIG_1fb(f))
      Vector(a, b).toARR
    )
  )

  def group: ENV =
    env.mod2((x, y) => y.vec1(f => x.groupByM(SIG_2f1(f), SIG_1f1(f)).toMAP))

  def span: ENV = env.mod2((x, y) =>
    y.vec1(f =>
      val (a, b) = x.spanM(SIG_2fb(f), SIG_1fb(f))
      Vector(a, b).toARR
    )
  )

  def pack: ENV =
    env.mod2((x, y) => y.vec1(f => x.packWithM(SIG_2x2fb(f), SIG_2fb(f))))

  def union: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.unionWith(y, SIG_2fb(f))))
  def intersect: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.intersectWith(y, SIG_2fb(f))))
  def diff: ENV =
    env.mod3((x, y, z) => z.vec1(f => x.differWith(y, SIG_2fb(f))))

  def evalTASK: ENV = env.arg1((x, env) =>
    env.push(x.vec1(f => TASK(Task.eval(env.push(f).quar.getStack(0)))))
  )
  def await: ENV =
    env.vec1(x => Await.result(x.toFUT.x, Duration.Inf))
  def awaitTRY: ENV =
    env.vec1(x => Await.ready(x.toFUT.x, Duration.Inf).value.get.toTRY)
  def toFUT: ENV = env.vec1(_.toFUT)
  def cancelFUT: ENV = env.arg1((x, env) =>
    x.vec1(_.toFUT.x.cancel().pipe(_ => UN))
    env
  )
  def forkTASK: ENV     = env.vec1(_.modTASK(_.executeAsync))
  def memoTASK: ENV     = env.vec1(_.modTASK(_.memoize))
  def memoTASK$ : ENV   = env.vec1(_.modTASK(_.memoizeOnSuccess))
  def uncancelTASK: ENV = env.vec1(_.modTASK(_.uncancelable))
  def timeoutTASK: ENV = env.vec2((x, n) =>
    val n1 = n.toNUM.x.toLong
    x.modTASK(
      _.timeoutWith(n1.milliseconds, LinEx("TASK", s"timeout after ${n1}ms"))
    )
  )
  def itrTASKW(t: ANY, f: Iterable[Task[ANY]] => Task[Iterable[ANY]]): ANY =
    t match
      case MAP(x) =>
        x.values
          .map(_.toTASK.x)
          .pipe(f)
          .map(_.zip(x.keys).map { case (v, k) => (k, v) }.to(VectorMap).toMAP)
          .pipe(TASK(_))
      case Itr(x) =>
        x.toSEQ.x
          .map(_.toTASK.x)
          .pipe(f)
          .map(_.toSEQ.matchType(x))
          .pipe(TASK(_))
      case x => x.toTASK
  def seqTASK: ENV = env.mod1(itrTASKW(_, Task.sequence))
  def parTASK: ENV = env.mod1(itrTASKW(_, Task.parSequence))
  def parnTASK: ENV =
    env.mod2((x, n) => itrTASKW(x, Task.parSequenceN(n.toInt)))
  def parunTASK: ENV = env.mod1(itrTASKW(_, Task.parSequenceUnordered))
  def raceTASK: ENV =
    env.mod1(_.toSEQ.x.map(_.toTASK.x).pipe(Task.raceMany).pipe(TASK(_)))

  def sleep: ENV = env.vec1(n =>
    val n1 = n.toNUM.x.toLong
    n1.milliseconds.pipe(Task.sleep).map(_ => NUM(n1)).pipe(TASK(_))
  )

  def dot: ENV = env.code.x match
    case c #:: cs =>
      env
        .modCode(_ => cs)
        .pipe(env =>
          c match
            case STR(x) => env.push(STR(StringContext.processEscapes(x)))
            case CMD(x) => env.wrapFN.push(CMD(x)).add$$
            case _      => env.push(c)
        )
    case _ => evalLNext
