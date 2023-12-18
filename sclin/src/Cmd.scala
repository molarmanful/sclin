package sclin

import better.files.*
import monix.execution.Scheduler.Implicits.global
import scala.annotation.switch
import scala.concurrent.duration.*
import spire.math.*
import ANY.*

extension (env: ENV)

  def cmd1(x: String): ENV = (x: @switch) match

    // CMDOC START

    /*
    @s ->
    Start `FN`.
     */
    case "(" => env.startFN
    /*
    @s -> FN
    End `FN`.
     */
    case ")" => env
    /*
    @s -> FN
    Empty `FN`.
     */
    case "()" => env.push(UN.toFN(env))
    /*
    @s -> FN
    #{)} #{,+}.
     */
    case ")+" => env.consFN
    /*
    @s a -> FN
    #{()} #{,+}.
     */
    case "()+" => env.push(UN.toFN(env)).consFN
    /*
    @s -> FN
    #{)} #{,+`}.
     */
    case ")++" => env.concatFN
    /*
    @s a -> FN
    #{()} #{,+`}.
     */
    case "()++" => env.push(UN.toFN(env)).concatFN
    /*
    @s -> TASK
    #{)} #{~Q}.
     */
    case ")~" => env.evalTASK
    /*
    @s -> TASK
    Empty `TASK`.
     */
    case "()~" => env.push(UN.toTASK)
    /*
    @s ->
    #{)} #{!Q}.
     */
    case ")!" => env.evalTRY
    /*
    @s -> TRY
    Empty `TRY`.
     */
    case "()!" => env.push(UN.toTRY)
    /*
    @s ->
    #{)} #{@$}.
     */
    case ")#"  => env.locId
    case "()#" => env
    /*
    @s ->
    #{)} #{@$$}.
     */
    case ")##"  => env.globId
    case "()##" => env
    /*
    @s ->
    #{)} #{->}.
     */
    case ")="  => env.lambda
    case "()=" => env
    /*
    @s ->
    Start `ARR`/`MAP`.
     */
    case "[" => env.startARR
    /*
    @s -> ARR
    End `ARR`.
     */
    case "]" => env.endARR
    /*
    @s -> ARR
    Empty `ARR`.
     */
    case "[]" => env.push(UN.toARR)
    /*
    @s -> STR
    #{]} #{>S}.
     */
    case "]S" => env.endARR.envSTR
    /*
    @s -> STR
    Empty `STR`.
     */
    case "[]S" => env.push(UN.toSTR)
    /*
    @s -> MAP
    End `MAP`.
     */
    case "]:" => env.endMAP
    /*
    @s -> MAP
    Empty `MAP`.
     */
    case "[]:" => env.push(UN.toMAP)
    /*
    @s -> MAP
    #{]} #{,>M}.
     */
    case "];"  => env.endARR.pairMAP
    case "[];" => env.push(UN.toMAP)
    /*
    @s -> _
    #{]} #{:/}.
     */
    case "/]" => env.endARR.getn
    /*
    @s -> _
    #{]} #{:/=}.
     */
    case "/]=" => env.endARR.setn
    /*
    @s -> _
    #{]} #{:/%}.
     */
    case "/]%" => env.endARR.setmodn
    /*
    @s -> _
    #{]:} #{:*}.
     */
    case "]*" => env.endMAP.gets
    /*
    @s -> _
    #{]:} #{:*=}.
     */
    case "]=" => env.endMAP.sets
    /*
    @s -> _
    #{]:} #{:*%}.
     */
    case "]%" => env.endMAP.setmods
    // TODO: elaborate
    /*
    @s ->
    Magic dot.
     */
    case "." => env.dot

    /*
    @s a -> STR
    Type of `a`.
     */
    case "type" => env.getType
    /*
    @s a -> STR
    `a` as formatted string.
     */
    case "form" => env.form
    /*
    @s a -> STR
    Converts `a` to `SEQ`.
     */
    case ">Q" => env.envSEQ
    /*
    @s a -> ARR
    Converts `a` to `ARR`.
     */
    case ">A" => env.envARR
    /*
    @s a -> ARR
    Converts `a` to `MAP`.
     */
    case ">M" => env.envMAP
    /*
    @s a' -> STR'
    Atomic #{>S}.
     */
    case "S" => env.vSTR
    /*
    @s a -> STR
    Converts `a` to `STR`.
     */
    case ">S" => env.envSTR
    /*
    @s a -> NUM
    Converts `a` to `NUM`.
     */
    case ">N" => env.envNUM
    /*
    @s a' -> NUM'
    Atomic #{>N}.
     */
    case "N" => env.vNUM
    /*
    @s a' -> DBL'
    Atomic #{>D}.
     */
    case "D" => env.vDBL
    /*
    @s a -> DBL
    Converts `a` to `DBL`.
     */
    case ">D" => env.envDBL
    /*
    @s a -> FN
    Converts `a` to `FN`.
     */
    case ">F" => env.envFN
    /*
    @s (a >STR) (b >STR) -> ERR
    Converts `a` to `ERR` with message `b`.
     */
    case ">E" => env.envERR
    /*
    @s a -> TASK
    Converts `a` to `TASK`.
     */
    case ">~" => env.envTASK
    /*
    @s a -> FUT
    Converts `a` to `FUT`.
     */
    case "~>" => env.envFUT
    /*
    @s a -> OBS
    Converts `a` to `OBS`.
     */
    case ">~`" => env.envOBS
    /*
    @s a -> TRY
    Converts `a` to `TRY`.
     */
    case ">!?" => env.envTRY
    /*
    @s a -> TRY
    Converts `a` to a `TRY` that succeeds.
     */
    case ">!+" => env.envYES
    /*
    @s a -> TRY
    Converts `a` to a `TRY` that fails.
     */
    case ">!-" => env.envNO
    /*
    @s a -> TF
    Converts `a` to `TF`.
     */
    case ">?" => env.envTF
    /*
    @s (a >OBS) -> TASK[TF]
    `OBS`-specific #{>?}.
     */
    case "~>?" => env.otoTF
    /*
    @s (a >NUM) (b >NUM)' -> STR
    Converts `a` to an `STR` formatted to `b`'s specifications.
     */
    case "N>d" => env.toNUMD
    /*
    @s a b -> _
    Converts `a` to type of `b`.
     */
    case ">TT" => env.matchType
    /*
    @s (a >STR)' -> MAP'
    #{>M} using a multiline string.
    Each line of `a` is #{#}ed, and the resulting top 2 stack items form each
    key-value pair.
    ```sclin
    `` >>M
    "a" 1
    "b" 2
    "c" 3
    `
    ```
     */
    case ">>M" => env.lineMAP
    /*
    @s a -> MAP
    #{>M} that first pairs elements of `a`.
     */
    case ",>M" => env.pairMAP
    /*
    @s (a >STR)' -> _'
    Converts `a` from JSON to `ANY`.
    ```sclin
    g; js>
    {"a": 1, "b": 2, "c": [3, 4]}
    ```
     */
    case "js>" => env.fromJSON
    /*
    @s a -> STR
    Converts `a` from `ANY` to JSON.
    ```sclin
    ["a" 1, "b" 2, "c" [3 4] , ]: >js
    ```
     */
    case ">js" => env.toJSON

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
    case "$LINE" => env.getLNum
    /*
    @s -> STR
    Current file of program execution.
     */
    case "$FILE" => env.getLFile
    /*
    @s -> STR
    Current working directory at start of program execution.
     */
    case "$PWD/" => env.push(Dsl.pwd.toString.sSTR)
    /*
    @s -> STR
    Current working directory at current state in program execution.
     */
    case "$CWD/" => env.push(Dsl.cwd.toString.sSTR)
    /*
    @s -> STR
    Home directory.
     */
    case "$~/" => env.push(File.home.toString.sSTR)
    /*
    @s -> SEQ[NUM*]
    Infinite `SEQ` of 0 to ∞.
     */
    case "$W" => env.push(wholes.toSEQ)
    /*
    @s -> SEQ[NUM*]
    Infinite `SEQ` of 1 to ∞.
     */
    case "$N" =>
      env.push(wholes.drop(1).toSEQ)
    /*
    @s -> SEQ[NUM*]
    Infinite `SEQ` of primes.
     */
    case "$P" => env.push(prime.lazyList.map(NUM(_)).toSEQ)
    /*
    @s -> ARR[STR*]
    `ARR` of lines of currently-executing file.
     */
    case "$L*" => env.getLns
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
    case "g@" => env.getLHere
    /*
    @s -> STR | UN
    Next line.
     */
    case "g;" => env.getLNext
    /*
    @s -> STR | UN
    Previous line.
     */
    case "g;;" => env.getLPrev
    /*
    @s -> STR
    Newline character.
     */
    case "n\\" => env.push(STR("\n"))
    /*
    @s -> NUM
    Number of milliseconds since UNIX epoch (January 1, 1970 00:00:00 UTC).
     */
    case "$NOW" => env.push(NUM(global.clockRealTime(MILLISECONDS)))
    /*
    @s -> STR
    Name of current thread.
     */
    case "$THREAD" => env.push(STR(Thread.currentThread.getName))
    /*
    @s -> MAP[(STR => STR)*]
    Environment variables.
     */
    case "$ENV" => env.push(sys.env.map(a => (a._1.sSTR, a._2.sSTR)).toMAP)
    /*
    @s -> MAP[(STR => _)*]
    Current scope.
     */
    case "$" => env.getscope

    /*
    @s a -> _
    Imports top of the stack from another sclin file.
     */
    case "@>" => env.importQ

    /*
    @s (a >FN)' ->
    Loads ID `a` into local scope.
    ```sclin
    "outer"=$a ( \a @$ a ) # $a
    #a "inner"
    ```
     */
    case "@$" => env.locId
    /*
    @s (a >FN) ->
    Loads ID `a` into global scope.
    ```sclin
    \a @$$ ( "inner" =$a $a ) # a
    #a "outer"
    ```
     */
    case "@$$" => env.globId
    /*
    @s (a >FN) -> STR | UN
    #{@$} and get as `STR`.
     */
    case "@:" => env.locIdS
    /*
    @s (a >FN) -> STR | UN
    #{@$$} and get as `STR`.
     */
    case "@::" => env.globIdS
    /*
    @s (a >FN) -> FN | UN
    #{@$} and get as `FN`.
     */
    case "@;" => env.locIdF
    /*
    @s (a >FN) -> FN | UN
    #{@$$} and get as `FN`.
     */
    case "@;;" => env.globIdF
    /*
    @s x* (a >FN) -> _*
    #{@;} and #{#}
     */
    case "@#" => env.locIdF.eval
    /*
    @s x* (a >FN) -> _*
    #{@;;} and #{#}.
     */
    case "@##" => env.globIdF.eval
    /*
    @s _* (a >FN) -> _*
    Stores stack items into local variables defined by `a`.
    Somewhat analogous to function arguments in other languages.
    ```sclin
    1 2 3 ;
    ( a b c ) -> $c $b $a
    ```
     */
    case "->" => env.lambda
    /*
    @s ->
    Clears local scope.
     */
    case "_$" => env.clrscope
    /*
    @s (a >MAP[(STR => _)*]) ->
    Appends `a` to the local scope.
     */
    case "+$" => env.setscope
    /*
    @s (a >MAP[(STR => _)*]) ->
    #{_$} and #{+$}.
     */
    case ">$" => env.clrscope.setscope

    /*
    @s -> STR
    Line from STDIN.
     */
    case "i>" => env.in
    /*
    @s (a >STR) ->
    Sends `a` to STDOUT.
     */
    case ">o" => env.out
    /*
    @s (a >STR) ->
    #{>o}s `a` with trailing newline.
     */
    case "n>o" => env.outn
    /*
    @s a ->
    #{form}s and #{n>o}s `a`.
     */
    case "f>o" => env.outf

    /*
    @s a -> a a
     */
    case "dup" => env.dup
    /*
    @s a* -> a* ARR[a*]
     */
    case "dups" => env.dups
    /*
    @s a b -> a a b
     */
    case "dupd" => env.dupd
    /*
    @s a b -> a b a
     */
    case "over" => env.over
    /*
    @s a b -> a b a b
     */
    case "ddup" => env.ddup
    /*
    @s a b c -> a b c a b c
     */
    case "edup" => env.edup
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
    case "pick" => env.pick
    /*
    @s _ ->
     */
    case "pop" => env.pop
    /*
    @s _* ->
     */
    case "clr" => env.clr
    /*
    @s _ b -> b
     */
    case "nip" => env.nip
    /*
    @s _ _ ->
     */
    case "ppop" => env.pop.pop
    /*
    @s _ _ _ ->
     */
    case "qpop" => env.pop.pop.pop
    /*
    @s (a @ n) b* (n >NUM) -> _*
    #{pop}s `n`th item from top of stack.
     */
    case "nix" => env.nix
    /*
    @s a b -> b a
     */
    case "swap" => env.swap
    /*
    @s a* -> _*
    Reverses stack.
     */
    case "rev" => env.rev
    /*
    @s a b c -> b a c
     */
    case "swapd" => env.swapd
    /*
    @s a b -> b a b
     */
    case "tuck" => env.tuck
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
    case "trade" => env.trade
    /*
    @s a b c -> b c a
     */
    case "rot" => env.rot
    /*
    @s a b c -> c a b
     */
    case "rot_" => env.rotu
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
    case "roll" => env.roll
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
    case "roll_" => env.rollu
    /*
    @s a* b (f >FN) -> _* b
    #{pop}s `b`, #{#}s `f`, and pushes `b`.
     */
    case "dip" => env.dip
    /*
    @s a -> FN[a]
    Wraps `a` in `FN`.
     */
    case "\\" => env.wrapFN
    /*
    @s a* f -> _*
    Executes `f`.
    ```sclin
    1 2 ( 3 + 4 ) #
    ```
     */
    case "#" => env.eval
    /*
    @s f' -> _'
    Evaluates `f` (#{#} but only preserves resulting top of stack).
    ```sclin
    1 2 ( dups 3+` ) Q
    ```
     */
    case "Q" => env.quar
    /*
    @s a* (n >NUM) -> _*
    #{#}s `n`th line.
     */
    case "@@" => env.evalLine
    /*
    @s a* (n >NUM) -> _*
    #{#}s `n`th line relative to current line.
     */
    case "@~" => env.evalLRel
    /*
    @s a* -> _*
    #{#}s current line.
     */
    case "@" => env.evalLHere
    /*
    @s a* -> _*
    #{#}s next line.
     */
    case ";" => env.evalLNext
    /*
    @s a* -> _*
    #{#}s previous line.
     */
    case ";;" => env.evalLPrev
    /*
    @s a* -> _*
    #{#}s first subsequent line whose leading whitespace count <= current line's leading whitespace count.
     */
    case ";>" => env.evalLNextInd
    /*
    @s a* -> _*
    #{#}s first preceding line whose leading whitespace count <= current line's leading whitespace count.
     */
    case ";;>" => env.evalLPrevInd
    /*
    @s (n >NUM) -> STR | UN
    `n`th line.
     */
    case "g@@" => env.getLn
    /*
    @s (n >NUM) -> STR | UN
    `n`th line relative to current line.
     */
    case "g@~" => env.getLRel
    /*
    @s a* (b >TF) f -> _*
    #{#}s `f` if `b` is truthy.
     */
    case "&#" => env.evalAnd
    /*
    @s a* (b >TF) f -> _*
    Non-destructive #{&#};
    executes `f` on `b` if `b` is truthy,
    otherwise keeps `b` on stack.
     */
    case "&&#" => env.evalAnd$
    /*
    @s a* (b >TF) f -> _*
    #{#}s `f` if `b` is falsy.
     */
    case "|#" => env.evalOr
    /*
    @s a* (b >TF) f -> _*
    Non-destructive #{|#};
    executes `f` on `b` if `b` is falsy,
    otherwise keeps `b` on stack.
     */
    case "||#" => env.evalOr$
    /*
    @s a* (b >TF) f g -> _*
    #{#}s `f` if `b` is truthy; else #{#}s `g`.
     */
    case "?#" => env.evalIf
    /*
    @s a* (b >MAP) -> _*
    Iterates through each key-value pair of `b`.
    For each pair: if the #{Q} of the key is truthy, then #{#}s the value and
    short-circuits.
     */
    case "??#" => env.evalIf$
    /*
    @s a* f (n >NUM) -> _*
    #{#}s `f` `n` times.
     */
    case "*#" => env.evalTimes
    /*
    @s a* f g -> _*
    Tries to #{#} `f`; on error, pushes caught `ERR` and #{#}s `g`.
     */
    case "!#" => env.evalTry
    /*
    @s f' -> TRY'
    #{Q}s `f` and wraps the result in a `TRY`.
     */
    case "!Q" => env.evalTRY
    /*
    @s f' -> TASK'
    Wraps `f` in a delayed, potentially asynchronous computation.
     */
    case "~Q" => env.evalTASK
    /*
    @s (e ERR) ->
    Throws `e`.
     */
    case ">!" => env.throwERR
    /*
    @s (a >ARR) f -> ARR
    #{#}s `f` on `a` as if it were a stack.
    ```sclin
    [1 2 3 4] ( 5 swap ) '
    ```
     */
    case "'" => env.evalArrSt
    /*
    @s (a* >ARR) f -> _*
    #{#}s `f` on the stack as if it were an `ARR`.
    ```sclin
    1 2 3 4 1.+.map '_
    ```
     */
    case "'_" => env.evalStArr
    /*
    @s ->
    Clears code queue, similar to the "break" keyword in other languages.
     */
    case "end" => env.end
    /*
    @s a ->
    Clears code queue and #{#}s top of stack.
     */
    case "_#" => env.evalEnd

    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a * 10 ^ b`
     */
    case "E" => env.scale
    /*
    @s (a >NUM)' -> NUM'
    Rounds `a` towards 0.
     */
    case "I" => env.trunc
    /*
    @s (a >NUM)' -> TF'
    Whether `a` is an integer.
     */
    case "I?" => env.isInt
    /*
    @s (a >NUM)' -> NUM'
    Rounds `a` towards -∞.
     */
    case "|_" => env.floor
    /*
    @s (a >NUM)' -> NUM'
    Rounds `a` to nearest integer.
     */
    case "|~" => env.round
    /*
    @s (a >NUM)' -> NUM'
    Rounds `a` towards ∞.
     */
    case "|^" => env.ceil
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
    case "X>b" => env.fromDec
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
    case "b>X" => env.toDec
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
    case ">n/d" => env.toNumDen
    /*
    @s (a >NUM)' -> TF'
    Whether `a` is an exact value (i.e. represented in full precision).
    ```sclin
    2 3/ prec?
    ```
    ```sclin
    $PI prec?
    ```
     */
    case "prec?" => env.isExact

    /*
    @s (a >NUM)' -> NUM'
    `-a`
     */
    case "_" => env.neg
    /*
    @s (a >STR)' -> STR'
    Atomic #{_`}.
     */
    case "__" => env.neg$
    /*
    @s a -> _
    Reverses `a`.
     */
    case "_`" => env.neg$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a + b`
     */
    case "+" => env.add
    /*
    @s (a >STR)' (b >STR)' -> STR'
    Atomic #{+`}.
     */
    case "++" => env.add$
    /*
    @s a b -> _
    Concatenates `a` and `b`.
     */
    case "+`" => env.add$$
    /*
    @s a b[_*] -> [a b*]
    Prepends `a` to `b`.
     */
    case "<+" => env.cons
    /*
    @s a[_*] b -> [a* b]
    Appends `b` to `a`.
     */
    case "+>" => env.snoc
    /*
    @s [a, b*] -> a [b*]
    Uncons; push first item and rest of `a`.
     */
    case "+<" => env.uncons
    /*
    @s a -> [_*] _
    Unsnoc; push last item and rest of `a`.
     */
    case ">+" => env.unsnoc
    /*
    @s a (b >FN) -> FN
    `FN`-specific #{<+}.
    Wraps `a` in `ARR` and uses #{,_} to preserve `a`'s value.'
     */
    case ",+" => env.consFN
    /*
    @s a (b >FN) -> FN
    `FN`-specific #{+`}.
    Uses #{,_} to preserve the values of `a`'s elements.'
     */
    case ",+`" => env.concatFN
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a - b`
     */
    case "-" => env.sub
    /*
    @s (a >STR)' (b >STR)' -> STR'
    Atomic #{-`}.
     */
    case "--" => env.sub$
    /*
    @s a b -> _
    Remove occurrences of `b` from `a`.
    If `a` is `MAP`, then removal is performed on keys instead of values.
    ```sclin
    [1 2 3 4] 2-`
    ```
    ```sclin
    [0 1, 2 3, ]: 2-`
    ```
     */
    case "-`" => env.sub$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a * b`
     */
    case "*" => env.mul
    /*
    @s (a >STR)' (b >NUM)' -> STR'
    Atomic #{*`}.
     */
    case "**" => env.mul$
    /*
    @s a b -> _
    `a` replicated according to `b`.
    If `b` is iterable, then `a` and `b` are recursively zipped together and
    replicated.
    ```sclin
    [1 2 3 4] [0 2 0 3] *` >A
    ```
    ```sclin
    [1 2 3 4] 3*` >A
    ```
     */
    case "*`" => env.mul$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a / b`. Throws error if `b` is 0.
     */
    case "/" => env.div
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Integer #{/}.
     */
    case "/~" => env.divi
    /*
    @s (a >STR)' (b >NUM)' -> SEQ[STR*]'
    Atomic #{/`}.
     */
    case "//" => env.div$
    /*
    @s a (b >NUM)' -> SEQ
    `a` chunked to size `b`.
    ```sclin
    [1 2 3 4 5] 2/` >A
    ```
     */
    case "/`" => env.div$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a (mod b)`
     */
    case "%" => env.mod
    /*
    @s (a >NUM)' (b >NUM)' -> NUM' NUM'
    Results of #{/~} and #{%} on `a` and `b`.
     */
    case "/%" => env.divmod
    /*
    @s (a >STR)' (b >NUM)' -> SEQ[STR*]'
    Atomic #{%`}.
     */
    case "%%" => env.mod$
    /*
    @s a (b >NUM)' -> SEQ
    `a` windowed to size `b`.
    ```sclin
    [1 2 3 4 5] 3%` >A
    ```
     */
    case "%`" => env.mod$$
    /*
    @s a (b >NUM)' (c >NUM)' -> SEQ
    #{%`} with extra skip parameter `c`.
    ```sclin
    [1 2 3 4 5] 3 2%` >A
    ```
     */
    case "%`~" => env.mod2$$
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    `a ^ b`. Throws error if result would be a complex number.
     */
    case "^" => env.pow
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    #{^} but `b` is coerced to `int`.
     */
    case "^~" => env.powi
    /*
    @s (a >STR)' (b >NUM)' -> SEQ[STR*]'
    Atomic #{^`}.
     */
    case "^^" => env.pow$
    /*
    @s a (n >NUM)' -> SEQ'
    Cartesian power of seed `a` to `n`.
    ```sclin
    "abc" 3^` >A
    ```
     */
    case "^`" => env.pow$$

    /*
    @s (a >NUM)' -> NUM'
    `e ^ a`
     */
    case "e^" => env.exp
    /*
    @s (a >NUM)' -> NUM'
    Absolute value of `a`.
     */
    case "abs" => env.abs
    /*
    @s (a >NUM)' -> NUM'
    Sine of `a`.
     */
    case "sin" => env.sin
    /*
    @s (a >NUM)' -> NUM'
    Cosine of `a`.
     */
    case "cos" => env.cos
    /*
    @s (a >NUM)' -> NUM'
    Tangent of `a`.
     */
    case "tan" => env.tan
    /*
    @s (a >NUM)' -> NUM'
    Arcsine of `a`.
     */
    case "sin_" => env.asin
    /*
    @s (a >NUM)' -> NUM'
    Arccosine of `a`.
     */
    case "cos_" => env.acos
    /*
    @s (a >NUM)' -> NUM'
    Arctangent of `a`.
     */
    case "tan_" => env.atan
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Arctangent of `a` with `b` as quadrant.
     */
    case "tan_II" => env.atan2
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic sine of `a`.
     */
    case "sinh" => env.sinh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic cosine of `a`.
     */
    case "cosh" => env.cosh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic tangent of `a`.
     */
    case "tanh" => env.tanh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic arcsine of `a`.
     */
    case "sinh_" => env.asinh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic arccosine of `a`.
     */
    case "cosh_" => env.acosh
    /*
    @s (a >NUM)' -> NUM'
    Hyperbolic arctangent of `a`.
     */
    case "tanh_" => env.atanh
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Base `b` logarithm of `a`.
     */
    case "log" => env.log
    /*
    @s (a >NUM)' -> NUM'
    Natural logarithm of `a`.
     */
    case "ln" => env.ln
    /*
    @s (a >NUM)' -> NUM'
    Base-10 logarithm of `a`.
     */
    case "logX" => env.log10
    /*
    @s (a >NUM)' -> NUM'
    Whether `a` is prime. Uses a strong pseudo-primality test with a 1/1e12
    chance of being wrong.
     */
    case "P?" => env.isPrime
    /*
    @s (a >NUM)' -> MAP[(NUM => NUM)*]
    Prime-factorizes `a` into pairs of prime `y` and frequency `z`.
    ```sclin
    340P/
    ```
     */
    case "P/" => env.factor

    /*
    @s (a >TF)' -> TF'
    Atomic #{!`}.
     */
    case "!" => env.not
    /*
    @s (a >TF) -> TF
    Logical NOT.
     */
    case "!`" => env.not$$
    /*
    @s a' b' -> (a | b)'
    Atomic #{&`}.
     */
    case "&" => env.min
    /*
    @s (a >TF)' (b >TF)' -> TF'
    Atomic #{&&`}.
     */
    case "&&" => env.and
    /*
    @s a b -> a | b
    Minimum of `a` and `b`.
     */
    case "&`" => env.min$$
    /*
    @s (a >TF) (b >TF) -> TF
    Logical AND of `a` and `b`.
     */
    case "&&`" => env.and$$
    /*
    @s a' b' -> (a | b)'
    Atomic #{|`}.
     */
    case "|" => env.max
    /*
    @s (a >TF)' (b >TF)' -> TF'
    Atomic #{||`}.
     */
    case "||" => env.or
    /*
    @s a b -> a | b
    Maximum of `a` and `b`.
     */
    case "|`" => env.max$$
    /*
    @s (a >TF) (b >TF) -> TF
    Logical OR of `a` and `b`.
     */
    case "||`" => env.or$$
    /*
    @s a' b' -> (-1 | 0 | 1)'
    Atomic #{<=>`}.
     */
    case "<=>" => env.cmp
    /*
    @s a b -> -1 | 0 | 1
    Comparison (-1, 0, or 1 depending on whether `a` is less than, equal to, or
    greater than `b`).
     */
    case "<=>`" => env.cmp$$
    /*
    @s a' b' -> TF'
    Atomic #{=`}.
     */
    case "=" => env.eql
    /*
    @s a b -> TF
    Whether `a` loosely equals `b`.
     */
    case "=`" => env.eql$$
    /*
    @s a' b' -> TF'
    Atomic #{==`}.
     */
    case "==" => env.eqls
    /*
    @s a b -> TF
    Whether `a` strictly equals `b`.
     */
    case "==`" => env.eql$$
    /*
    @s a' b' -> TF'
    Atomic #{!=`}.
     */
    case "!=" => env.neq
    /*
    @s a b -> TF
    Whether `a` does not loosely equal `b`.
     */
    case "!=`" => env.neq$$
    /*
    @s a' b' -> TF'
    Atomic #{!=`}.
     */
    case "!==" => env.neqs
    /*
    @s a b -> TF
    Whether `a` does not loosely equal `b`.
     */
    case "!==`" => env.neqs$$
    /*
    @s a' b' -> TF'
    Atomic #{<`}.
     */
    case "<" => env.lt
    /*
    @s a b -> TF
    Whether `a` is less than `b`.
     */
    case "<`" => env.lt$$
    /*
    @s a' b' -> TF'
    Atomic #{>`}.
     */
    case ">" => env.gt
    /*
    @s a b -> TF
    Whether `a` is greater than `b`.
     */
    case ">`" => env.gt$$
    /*
    @s a' b' -> TF'
    Atomic #{<=`}.
     */
    case "<=" => env.lteq
    /*
    @s a b -> TF
    Whether `a` is less than or equal to `b`.
     */
    case "<=`" => env.gt$$
    /*
    @s a' b' -> TF'
    Atomic #{>=`}.
     */
    case ">=" => env.gteq
    /*
    @s a b -> TF
    Whether `a` is greater than or equal to `b`.
     */
    case ">=`" => env.gt$$

    /*
    @s (a >NUM)' -> NUM'
    Bitwise NOT.
     */
    case "b~" => env.bNOT
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Bitwise AND.
     */
    case "b&" => env.bAND
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Bitwise OR.
     */
    case "b|" => env.bOR
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Bitwise XOR.
     */
    case "b^" => env.bXOR
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Bitwise LSHIFT.
     */
    case "b<<" => env.bLSH
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Bitwise RSHIFT.
     */
    case "b>>" => env.bRSH
    /*
    @s (a >NUM)' (b >NUM)' -> NUM'
    Bitwise unsigned RSHIFT.
     */
    case "b>>>" => env.bURSH

    /*
    @s a i' -> (a._ | UN)'
    Value at atomic index `i` in `a`.
     */
    case ":" => env.get
    /*
    @s a -> a._
    Value at random index in `a`.
     */
    case ":r" => env.getr
    /*
    @s a i -> a._ | UN
    Value at index `i` in `a`.
     */
    case ":`" => env.get$$
    /*
    @s a (i >SEQ) -> _
    #{:`} with `i` mapped over `a`.
     */
    case ":*" => env.gets
    /*
    @s a (i >SEQ) -> _
    #{:`} with `i` folded over `a`.
     */
    case ":/" => env.getn
    /*
    @s a b i -> _
    Sets value at index `i` in `a` to `b`.
     */
    case ":=" => env.set
    /*
    @s a (m >MAP) -> _
    #{:=} with `i` mapped over `a`.
     */
    case ":*=" => env.sets
    /*
    @s a b (i >SEQ) -> _
    #{:=} with `i` folded over `a`.
     */
    case ":/=" => env.setn
    /*
    @s a (f >FN) i -> _
    Modifies value at index `i` using `f`.
     */
    case ":%" => env.setmod
    /*
    @s a (m >MAP[(_ => (_ >FN))*]) -> _
    #{:%} with `i` mapped over `a`.
     */
    case ":*%" => env.setmods
    /*
    @s a (f >FN) (i >SEQ) -> _
    #{:%} with `i` folded over `a`.
     */
    case ":/%" => env.setmodn
    /*
    @s a i -> _
    Removes index `i` from `a`.
     */
    case ":-" => env.idel
    /*
    @s a b' -> TF'
    Whether `a` has atomic `b`.
     */
    case ":?" => env.has
    /*
    @s a b -> TF
    Whether `a` has `b`.
    `MAP`s check `b` against keys; other types of `a` check `b` against values.
     */
    case ":?`" => env.has$$
    /*
    @s a -> NUM
    Length of `a`.
     */
    case "len" => env.len
    /*
    @s (a >OBS) -> TASK[NUM]
    `OBS`-specific #{len}.
     */
    case "~len" => env.olen
    /*
    @s a -> ARR[NUM*]
    Shape of `a`, i.e. #{len} of each dimension of `a`.
    Determined by first element of each dimension.
     */
    case "shape" => env.shape
    /*
    @s a -> ARR[NUM*]
    #{shape} but recursively maximizes lengths and depth instead of just using
    the first element.
     */
    case "shape^" => env.dshape
    // TODO: docs
    case "shape=" => env.reshape
    /*
    @s a -> NUM
    #{len} of #{shape} of `a`.
     */
    case "rank" => env.rank
    /*
    @s a -> NUM
    #{len} of #{shape^} of `a`.
     */
    case "rank^" => env.depth
    /*
    @s a b -> ARR[a b]
    Pairs `a` and `b` in an `ARR`.
     */
    case "," => env.wrap$
    /*
    @s a -> ARR[a]
    Wraps `a` in an `ARR`.
     */
    case ",," => env.wrap
    /*
    @s a' b' -> ARR[a b]'
    Vectorized #{,}.
     */
    case ",'" => env.wrapv$
    /*
    @s a' -> ARR[a]'
    Vectorized #{,,}.
     */
    case ",,'" => env.wrapv
    /*
    @s a* -> a
    Wraps stack in an `ARR`.
     */
    case ",`" => env.wrap$$
    /*
    @s a -> a*
    Unwraps `a`.
     */
    case ",_" => env.unwrap
    /*
    @s _* a -> a*
    Replaces stack with `a` unwrapped.
     */
    case ",,_" => env.unwrap$
    /*
    @s a (n >NUM)' -> _
    Takes up to `n` items from `a`.
    Negative `n` takes from the end instead of the start.
     */
    case "tk" => env.tk
    /*
    @s (a >OBS) (n >NUM)' -> OBS
    Emits `a` for `n` milliseconds.
     */
    case "~tk" => env.otk
    /*
    @s (a >OBS) (b >OBS) -> OBS
    Emits `a` until `b` completes.
     */
    case "~tk`" => env.otko
    /*
    @s a (n >NUM)' -> _
    Drops up to `n` items from `a`.
    Negative `n` drops from the end instead of the start.
     */
    case "dp" => env.dp
    /*
    @s (a >OBS) (b >OBS) -> OBS
    Discards emissions of `a` for `n` milliseconds.
     */
    case "~dp" => env.odp
    /*
    @s (a >OBS) (b >OBS) -> OBS
    Discards emissions of `a` until `b` completes.
     */
    case "~dp`" => env.odpo
    /*
    @s a -> _
    Flattens `a` by one depth.
     */
    case "flat" => env.flat
    /*
    @s (a >OBS) -> OBS
    `OBS`-friendly #{flat}.
     */
    case "~flat" => env.merge
    /*
    @s a -> _
    Flattens `a` recursively.
     */
    case "rflat" => env.rflat
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
    case "rep" => env.rep
    /*
    @s a -> OBS
    `OBS`-specific #{rep}.
     */
    case "~rep" => env.orep
    /*
    @s a -> SEQ
    Infinite `SEQ` with elements of `a` cycled.
    ```sclin
    [1 2 3] cyc 10tk >A
    ```
     */
    case "cyc" => env.cyc
    /*
    @s a -> OBS
    `OBS`-specific #{cyc}.
     */
    case "~cyc" => env.ocyc
    /*
    @s (a >NUM)' -> ARR[1*]'
    Length-`a` `ARR` of 1's.
    ```sclin
    10I*
    ```
     */
    case "I*" => env.ones
    /*
    @s (a >ARR) -> ARR
    `ARR` of 1's with dimensions `a`.
    ```sclin
    [2 3 4] I^
    ```
     */
    case "I^" => env.one$
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
    case "mold" => env.toShape
    /*
    @s a (f: b -> _)' -> SEQ'
    Infinite `SEQ` of `f` successively #{Q}ed to `a`.
    ```sclin
    1 1.+ itr 10tk >A
    ```
    ```sclin
    1 ( 1+ 1 swap / ) itr 10tk >A
    ```
     */
    case "itr" => env.itr
    /*
    @s a (f: x -> >TASK)' -> OBS'
    `OBS`-specific #{itr}.
     */
    case "~itr" => env.oitr
    /*
    @s a (f: b -> _ _ | ) -> SEQ
    `SEQ` generated from `f` successively #{Q}ed to `a`, where `x` is the new
    current item and `y` is the next `b` to be subsequently #{Q}ed to `f`.
    Generation stops if `f` #{Q}ed to `a` results in an empty stack.
    ```sclin
    0 1, ( ,_ tuck + dups \swap dip ) fold_ 10tk >A
    ```
     */
    case "fold_" => env.unfold
    /*
    @s a (f: b -> >TASK[ARR[_ _ | ]] ) -> OBS
    `OBS`-specific #{fold_}.
    Note that unlike in #{fold_}, `f` should return an `ARR` which acts in place
    of the "stack".
     */
    case "~fold_" => env.ounfold
    /*
    @s a -> [ARR[k v]*]
    Key-value pairs of `a`.
    ```sclin
    ["a" "b" "c" "d"] >kv >A
    ```
    ```sclin
    ["x""a", "y""b", "z""c", ]: >kv >A
    ```
     */
    case ">kv" => env.enumL
    /*
    @s a -> MAP
    #{>kv} and #{>M}.
    ```sclin
    ["a" "b" "c" "d"] =>kv
    ```
     */
    case "=>kv" => env.enumL.envMAP
    /*
    @s a -> SEQ | ARR
    Keys in `a`.
    ```sclin
    ["x" "a", "y" "b", "z" "c", ]: >k >A
    ```
     */
    case ">k" => env.keys
    /*
    @s a -> SEQ | ARR
    Values in `a`.
    ```sclin
    ["x""a", "y""b", "z""c", ]: >v >A
    ```
     */
    case ">v" => env.vals
    /*
    @s (a >NUM)' (b >NUM)' -> ARR[NUM*]'
    Exclusive range from `a` to `b`.
     */
    case "a>b" => env.range
    /*
    @s (a >NUM)' (b >NUM)' -> ARR[NUM*]'
    Inclusive range from `a` to `b`.
     */
    case "a-b" => env.irange
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
    case "shuf" => env.shuffle
    /*
    @s a -> SEQ
    All permutations of `a`.
    ```sclin
    [1 2 3] perm >A
    ```
     */
    case "perm" => env.perm
    /*
    @s a (n >NUM)' -> SEQ'
    All length-`n` combinations of `a`.
    ```sclin
    [1 2 3] 2comb >A
    ```
     */
    case "comb" => env.comb
    /*
    @s a -> SEQ
    All subsets of `a`.
    ```sclin
    [1 2 3] ^set >A
    ```
     */
    case "^set" => env.powset
    /*
    @s a[_*] -> SEQ'
    Cartesian product of iterable-of-iterables `a` to `n`.
    ```sclin
    ["abc" "def" "ghi"] Q* >A
    ```
     */
    case "Q*" => env.cProd
    /*
    @s a[_*] -> [_*]
    Transposes a collection of collections matrix-style.
    Safe for infinite lists.
    ```sclin
    [[1 2 3][4 5 6][7 8 9]] tpose
    ```
    ```sclin
    [[1 2][3 4 5][6]] tpose
    ```
     */
    case "tpose" => env.transpose
    /*
    @s a[_*] -> [_*]
    Reverses axes of an N-dimensional collection.
     */
    case "raxes" => env.raxes
    /*
    @s a[_*] (b >ARR[NUM*]) -> [_*]
    Permute axes of an N-dimensional collection using `b`.
     */
    case "paxes" => env.paxes
    /*
    @s (a >STR)' (b >NUM)' (c >STR)' -> STR'
    Atomic #{pad`}.
     */
    case "pad" => env.pad
    /*
    @s (a >STR)' (b >NUM)' (c >STR)' -> STR'
    Atomic #{padl`}.
     */
    case "padl" => env.padl
    /*
    @s (a >STR)' (b >NUM)' (c >STR)' -> STR'
    Atomic #{padc`}.
     */
    case "padc" => env.padc
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
    case "pad`" => env.pad$
    /*
    @s a[_*] (b >NUM)' c -> STR'
    Pads `a` from the left to length `b` using `c`.
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
    case "padl`" => env.padl$
    /*
    @s a[_*] (b >NUM)' c -> STR'
    Pads `a` from the outsides to length `b` using `c`.
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
    case "padc`" => env.padc$

    /*
    @s (a >STR)' -> ARR[NUM*]'
    Converts `a` to codepoints.
    ```sclin
    "hello"S>c
    ```
     */
    case "S>c" => env.toCodePt
    /*
    @s (a >ARR[NUM*]) -> STR
    Converts iterable of codepoints to `STR`.
    ```sclin
    [104 101 108 108 111] c>S
    ```
     */
    case "c>S" => env.fromCodePt
    /*
    @s (a >STR)' (b >STR)' -> ARR'
    Splits `a` with `b`.
     */
    case "<>" => env.split
    /*
    @s a (i >NUM) -> ARR[_ _]
    #{tk} and #{dp} of `a` at index `i`.
     */
    case "<>:" => env.splitAt
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
    case "s<>" => env.ssplit
    /*
    @s a (b >STR)' -> STR'
    Joins `a` with `b`.
     */
    case "><" => env.join
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
    case "A>a" => env.toLower
    /*
    @s (a >STR)' -> STR'
    Converts `STR` to `UPPERCASE`.
     */
    case "a>A" => env.toUpper
    /*
    @s (a >STR)' -> STR'
    Converts `STR` to `Capitalized`.
     */
    case ">Aa" => env.toCap
    /*
    @s (a >STR)' (r >STR)' -> SEQ[MAP]'
    Matches `a` with regex `r`.
    Each match returned is a `MAP` with the following keys:
    - ``` & ```: Matched `STR`.
    - ``` ` ```: `STR` before the match.
    - ``` ' ```: `STR` after the match.
    - ``` * ```: `ARR[MAP*]` of each capturing group matched.
    - ``` ^ ```: `NUM` index of the match's start.
    - ``` $ ```: `NUM` index of the match's end.
     */
    case "/?" => env.rmatch
    /*
    @s (a >STR)' (r >STR)' -> SEQ[STR]'
    #{/?} with only `&` keys.
     */
    case "/?&" => env.rmatchMatch
    /*
    @s (a >STR)' (r >STR)' -> SEQ[STR]'
    #{/?} with only `'` keys.
     */
    case "/?`" => env.rmatchBefore
    /*
    @s (a >STR)' (r >STR)' -> SEQ[STR]'
    #{/?} with only ``` ` ``` keys.
     */
    case "/?'" => env.rmatchAfter
    /*
    @s (a >STR)' (r >STR)' -> SEQ[ARR[MAP*]]'
    #{/?} with only `*` keys.
     */
    case "/?*" => env.rmatchGroups
    /*
    @s (a >STR)' (b >STR)' -> SEQ[NUM]'
    #{/?} with only `^` keys.
     */
    case "/?^" => env.rmatchStart
    /*
    @s (a >STR)' (b >STR)' -> SEQ[NUM]'
    #{/?} with only `$` keys.
     */
    case "/?$" => env.rmatchEnd
    /*
    @s (a >STR)' (r >STR)' (f: MAP -> >STR)' -> STR'
    Replace matches of regex `r` on `a` by applying each match `MAP` to `f`.
     */
    case "/#" => env.rsub
    /*
    @s (a >STR)' (r >STR)' (s >STR)' -> STR'
    Replace first match of regex `r` on `a` with `s`.
     */
    case "/#^" => env.rsubFirst

    /*
    @s (a MAP) (f: k v -> _)' -> _'
    @s a (f: x -> _)' -> _'
    #{Q}s `f` on each element of `a`.
    ```sclin
    [1 2 3 4] 1.+ map
    ```
    ```sclin
    [0 1, 2 3, 4 5, ]: ( over + ) map
    ```
     */
    case "map" => env.map
    /*
    @s (a >OBS) (f: x -> >TASK)' -> OBS'
    `OBS`-specific #{map}.
     */
    case "~map" => env.mapEval
    /*
    @s (a >OBS) (f: x -> >TASK)' (n >NUM)' -> OBS[ARR]'
    #{~map} that executes `n` tasks in parallel; both effects and results are
    unordered.
     */
    case "~map||" => env.mapPar
    /*
    @s (a >OBS) (f: x -> >TASK)' (n >NUM)' -> OBS[ARR]'
    #{~map||} but results are ordered.
     */
    case "~map||>" => env.mapParOrd
    /*
    @s a (f: k v -> )' -> a'
    @s a (f: x -> )' -> a'
    #{map} but `a` is preserved (i.e. leaving only side effects of `f`).
    ```sclin
    [1 2 3 4] ( 1+ n>o ) tap
    ```
     */
    case "tap" => env.tapMap
    /*
    @s (a MAP) (f: k v -> )' -> UN'
    @s (a OBS) (f: x -> )' -> FUT[UN]'
    @s a (f: x -> )' -> UN'
    #{tap} that forces computation of `a` (e.g. with `SEQ` or `OBS`).
    ```sclin
    [1 2 3 4] ( 1+ n>o ) tap
    ```
     */
    case "each" => env.foreach
    /*
    @s (a MAP) (f: k v -> >A)' -> _'
    @s a (f: x -> _)' -> _'
    #{map} and flatten results based on `a`'s type.
    ```sclin
    1224P/ \*` mapf
    ```
     */
    case "mapf" => env.flatMap
    /*
    @s (a >OBS) (f: x -> >OBS)' -> OBS
    `OBS`-specific #{mapf}. Unlike #{mapf}, #{~mapf} is non-deterministic;
    execution occurs concurrently, and results are unordered.
     */
    case "~mapf" => env.mergeMap
    /*
    @s (a >OBS) (f: x -> >OBS)' -> OBS
    #{~mapf} that only keeps the first result.
     */
    case "~mapf~" => env.switchMap
    /*
    @s a f' (n >NUM)' -> _'
    `n`-wise reduction of `f` over `a`.
    ```sclin
    [1 2 3 4] \+ 2%map
    ```
     */
    case "%map" => env.winMap
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
    [1 2 3 4] [1 "a", 3 "b", "x" "c", ]: \, zip
    ```
     */
    case "zip" => env.zip
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
    [1 2 3 4] [1 "a", 3 "b", "x" "c", ]: UN UN \, zip~
    ```
     */
    case "zip~" => env.zip$
    /*
    @s a b (f: x y -> _)' -> _'
    #{Q}s `f` over each table-wise pair of `a` and `b`.
    ```sclin
    [1 2 3 4] [2 3 4 5] \++ tbl
    ```
     */
    case "tbl" => env.tbl
    /*
    @s a b (f: x y -> _)' -> _'
    #{tbl} and #{flat}.
     */
    case "tblf" => env.tblf
    /*
    @s a (f: x -> _)' -> _'
    Atomic/recursive #{map}.
    ```sclin
    [[1 2] 3 4 [5 [6 7]]] ( dup f>o ) rmap
    ```
     */
    case "rmap" => env.rmap
    /*
    @s a (f: x -> _)' (n >NUM)' -> _'
    #{map} at depth `n`.
    - If `n > 0`, then depth is calculated outer-to-inner.
    - If `n = 0`, then behavior is identical to #{rmap}.
    - If `n < 0`, then depth is calculated inner-to-outer.
    ```sclin
    [[1 2] 3 4 [5 [6 7]]] ( dup f>o ) 2dmap
    ```
    ```sclin
    [[1 2] 3 4 [5 [6 7]]] ( dup f>o ) 1_ dmap
    ```
     */
    case "dmap" => env.dmap
    /*
    @s (a MAP) b (f: k x v -> _)' -> _'
    @s a b (f: x y -> _)' -> _'
    #{Q}s `f` to combine accumulator and each element starting from initial accumulator `b`.
    ```sclin
    [1 2 3 4] 0 \+ fold
    ```
    ```sclin
    "1011"_` =>kv 0 ( rot 2 swap ^ * + ) fold
    ```
     */
    case "fold" => env.fold
    /*
    @s (a >OBS) b (f: x y -> _) -> TASK
    `OBS`-specific #{fold}.
     */
    case "~fold" => env.ofold
    /*
    @s (a MAP) b (f: k v x -> _)' -> _'
    @s a b (f: y x -> _)' -> _'
    #{fold} from the right.
     */
    case "foldr" => env.foldR
    /*
    @s a b (f: x y -> _)' -> _'
    Atomic/recursive #{fold}.
    ```sclin
    [[1 2] 3 4 [5 [6 7]]] 0 \+ rfold
    ```
    ```sclin
    [[1 2] 3 4 [5 [6 7]]] [] \+` rfold
    ```
     */
    case "rfold" => env.rfold
    /*
    @s a b (f: y x -> _)' -> _'
    #{rfold} from the right.
     */
    case "rfoldr" => env.rfoldR
    /*
    @s (a MAP) (f: k x v -> _)' -> _'
    @s a (f: x y -> _)' -> _'
    #{fold} without initial accumulator, instead using the first element of `a`.
    If `a` is empty, then an error is thrown.
    ```sclin
    [1 2 3 4] \+ fold~
    ```
    ```sclin
    [1 5 10 4 3] \| fold~
    ```
     */
    case "fold~" => env.reduce
    /*
    @s (a MAP) (f: k v x -> _)' -> _'
    @s a (f: y x -> _)' -> _'
    #{fold~} from the right.
     */
    case "foldr~" => env.reduceR
    /*
    @s (a MAP) (f: k x v -> _)' -> _'
    @s a b (f: x y -> _)' -> _'
    #{fold} with intermediate values.
    ```sclin
    [1 2 3 4] 0 \+ scan
    ```
     */
    case "scan" => env.scan
    /*
    @s (a >OBS) (b >TASK) (f: x y -> >TASK) -> OBS
    `OBS`-specific #{scan}.
     */
    case "~scan" => env.scanEval
    /*
    @s (a MAP) (f: k v x -> _)' -> _'
    @s a b f' -> _'
    #{scan} from the right.
     */
    case "scanr" => env.scanR
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
    A multi-purpose function for creating, modifying, and traversing nested
    structures.
    ```sclin
    [[1 2] 3 4 [ "a" 5, "b" [6 7] , ]: ] ( dups f>o ) walk
    ```
    ```sclin
    [[1 2] 3 4 [ "a" 5, "b" [6 7] , ]: ] ( dup len 0> ( dup +` ) &# ) walk
    ```
     */
    case "walk" => env.walk
    /*
    @s (a MAP) (f: k v -> >TF)' -> _'
    @s a (f: x -> >TF)' -> _'
    Keeps elements of `a` that satisfy predicate `f`.
    ```sclin
    [5 1 2 4 3] 2.> fltr
    ```
     */
    case "fltr" => env.fltr
    /*
    @s (a >OBS) (f: x -> >TASK[TF]) -> OBS
    `OBS`-specific #{fltr}.
     */
    case "~fltr" => env.ofltr
    /*
    @s (a MAP) (f: k v -> >TF)' -> TF'
    @s a (f: x -> >TF)' -> TF'
    Whether any elements of `a` satisfy predicate `f`.
    ```sclin
    [5 1 2 4 3] 2.> any
    ```
     */
    case "any" => env.any
    /*
    @s (a >OBS) (f: x -> >TASK[TF]) -> TASK[TF]
    `OBS`-specific #{any}.
     */
    case "~any" => env.oany
    /*
    @s (a MAP) (f: k v -> >TF)' -> TF'
    @s a (f: x -> >TF)' -> TF'
    Whether all elements of `a` satisfy predicate `f`.
    ```sclin
    [5 1 2 4 3] 2.> all
    ```
     */
    case "all" => env.all
    /*
    @s (a >OBS) (f: x -> >TASK[TF]) -> TASK[TF]
    `OBS`-specific #{all}.
     */
    case "~all" => env.all
    /*
    @s (a MAP) (f: k v -> >TF)' -> _'
    @s a (f: x -> >TF)' -> _'
    Takes elements of `a` until #{Q}ing `f` is falsy.
    ```sclin
    [5 1 2 4 3] 4.!= tk*
    ```
     */
    case "tk*" => env.tkwl
    /*
    @s (a MAP) (f: k v -> >TF)' -> _'
    @s a (f: x -> >TF)' -> _'
    Drops elements of `a` while predicate `f` is truthy.
    ```sclin
    [5 1 2 4 3] 4.!= dp*
    ```
     */
    case "dp*" => env.dpwl
    /*
    @s (a MAP) (f: k v -> >TF)' -> _'
    @s a (f: x -> >TF)' -> _'
    Finds first element of `a` where predicate `f` is truthy.
    Returns `UN` if not found.
    ```sclin
    [5 1 2 4 3] ( 2% ! ) find
    ```
     */
    case "find" => env.find
    /*
    @s (a >OBS) (f: x -> >TASK[TF]) -> TASK
    `OBS`-specific #{find}.
     */
    case "~find" => env.ofind
    /*
    @s (a MAP) (f: k v -> >TF)' -> NUM'
    @s a (f: x -> >TF)' -> NUM'
    Finds index of first element of `a` where predicate `f` is truthy.
    Returns `-1` if not found.
    ```sclin
    [5 1 2 4 3] ( 2% ! ) find:
    ```
     */
    case "find:" => env.findi
    /*
    @s (a MAP) (f: k v -> >TF)' -> _'
    @s a (f: x -> >TF)' -> _'
    Deletes first element of `a` where predicate `f` is truthy.
    ```sclin
    [5 1 2 4 3] ( 2% ! ) del
    ```
     */
    case "del" => env.del
    /*
    @s (a MAP) (f: k v -> _)' -> _'
    @s a (f: x -> _)' -> _'
    Uniquifies elements of `a` with mapper `f`.
    ```sclin
    [2 4 3 3 5 4 1] () uniq
    ```
    ```sclin
    [5 1 2 4 3] 2.% uniq
    ```
     */
    case "uniq" => env.uniq
    /*
    @s (a MAP) (f: ARR[k v] ARR[j w] -> >TF)' -> _'
    @s a (f: x y -> >TF)' -> _'
    Uniquifies elements of `a` with equality predicate `f`.
    ```sclin
    [2 4 3 3 5 4 1] \=` uniq~
    ```
    ```sclin
    [2 4 3 3 5 4 1] 2.% uniq~
    ```
     */
    case "uniq~" => env.uniq$
    /*
    @s (a MAP) (f: k v -> _)' -> _'
    @s a (f: x -> _)' -> _'
    Sorts elements of `a` with mapper `f`.
    ```sclin
    ["a" "" "abc" "ab"] \len sort
    ```
    ```sclin
    [1 2 3 4 5] \$rng sort
    ```
     */
    case "sort" => env.sort
    /*
    @s (a MAP) (f: ARR[k v] ARR[j w] -> >NUM)' -> _'
    @s a (f: x y -> >NUM)' -> _'
    Sorts elements of `a` with comparator `f`.
    ```sclin
    [1 5 2 3 4] \< sort~
    ```
    ```sclin
    [1 5 2 3 4] \> sort~
    ```
     */
    case "sort~" => env.sort$
    /*
    @s (a MAP) (f: k v -> >TF)' -> ARR[_ _]'
    @s a (f: x -> >TF)' -> ARR[_ _]'
    Separates `a` into 2 parts based on predicate `f`.
    ```sclin
    [5 1 2 4 3] 2.> part
    ```
     */
    case "part" => env.part
    /*
    @s (a MAP) (f: k v -> _)' -> MAP'
    @s a (f: x -> _)' -> MAP'
    Separates `a` groups based on `f`.
    Each result of `f` becomes a key in the resulting `MAP`.
    ```sclin
    "abc"^set >A \len group
    ```
     */
    case "group" => env.group
    /*
    @s (a >OBS) (f: x -> _)' -> OBS[ARR[_ _]*]'
    `OBS`-specific #{group}.
     */
    case "~group" => env.ogroup
    /*
    @s (a MAP) (f: k v -> >TF)' -> ARR[_ _]'
    @s a (f: x -> >TF)' -> ARR[_ _]'
    Equivalent to a combination of #{tk*} and #{dp*}.
    ```sclin
    [5 1 2 4 3] 2.% span
    ```
     */
    case "span" => env.span
    /*
    @s (a MAP) (f: ARR[k v] ARR[j w] -> >TF)' -> _'
    @s a (f: x y -> >TF)' -> _'
    Groups consecutive duplicate runs of `a` based on predicate `f`.
    ```sclin
    [1 1 2 3 3 4 6 4 4] \=` pack
    ```
     */
    case "pack" => env.pack

    /*
    @s a b (f: x y -> >TF)' -> _'
    Gets the union of `a` and `b` with equality predicate `f`.
    ```sclin
    [1 2 3 4] [2 4 6 8] \=` union
    ```
     */
    case "union" => env.union
    /*
    @s a b (f: x y -> >TF)' -> _'
    Gets the intersection between `a` and `b` with equality predicate `f`.
    May hang if `a` or `b` are infinite.
    See #{sort~} for the signature of `f`.
    ```sclin
    [1 2 3 4] [2 4 6 8] \=` intxn
    ```
     */
    case "intxn" => env.intersect
    /*
    @s a b (f: x y -> >TF)' -> _'
    Gets the difference between `a` and `b` with equality predicate `f`.
    Will hang if `b` is infinite.
    See #{sort~} for the signature of `f`.
    ```sclin
    [1 2 3 4] [2 4 6 8] \=` diff
    ```
     */
    case "diff" => env.diff

    /*
    @s (a >FUT[x])' -> x'
    Synchronously waits for `a` to complete, leaving the result on the stack.
     */
    case "~_" => env.await
    /*
    @s (a >FUT)' ->
    Cancels `a`.
     */
    case "~$" => env.cancelFUT
    /*
    @s (a OBS) -> OBS
    @s (a >TASK)' -> TASK
    Inserts an asynchronous boundary.
     */
    case "~^" => env.asyncBound
    /*
    @s (a >OBS) (b >OSTRAT)' -> OBS'
    `OBS`-specific #{~^} that uses `b` to determine overflow strategy.
     */
    case "~^`" => env.oasyncBound
    /*
    @s a[>TASK*] -> TASK[[_*]]
    Executes each `TASK` in `a` sequentially such that both effects and results
    are ordered.
     */
    case "~|>" => env.seqTASK
    /*
    @s a[>TASK*] -> TASK[[_*]]
    Executes each `TASK` in `a` in parallel such that effects are unordered but
    results are ordered.
     */
    case "~||" => env.parTASK
    /*
    @s a[>TASK*] (n >NUM) -> TASK[[_*]]
    #{~||} but with at most `n` concurrently running `TASK`s.
     */
    case "~||>" => env.parnTASK
    /*
    @s a[>TASK*] -> TASK[[_*]]
    #{~||} but results are also unordered.
     */
    case "~//" => env.parunTASK
    /*
    @s a[>TASK*] -> TASK
    Races a collection of `TASK`s, returning the first to complete.
     */
    case "~>>" => env.raceTASK
    /*
    @s (a >TASK)' -> TASK'
    Ensures that `a` runs on a separate thread.
     */
    case "~<" => env.forkTASK
    /*
    @s (a >TASK)' -> TASK'
    Ensures that `a` runs on a thread-pool for blocking I/O.
     */
    case "~io" => env.forkIOTASK
    /*
    @s (a OBS) -> OBS
    @s (a >TASK)' -> TASK'
    Ensures that `a` is memoized such that subsequent runs return the same
    value.
     */
    case "~memo" => env.memoTASK
    /*
    @s (a >TASK)' -> TASK'
    #{~memo} but only if `a` completes successfully.
     */
    case "~memo&" => env.memoTASK$
    /*
    @s (a >OBS) (n >NUM)' -> OBS'
    Caches emissions of `a` with maximum cache capacity `n`.
     */
    case "~memo`" => env.ocache
    /*
    @s (a OBS) -> OBS
    @s (a >TASK)' -> TASK'
    Ensures that `a` is uncancellable.
     */
    case "~$_" => env.uncancelTASK
    /*
    @s (a >TASK)' (ms >NUM)' -> TASK'
    Ensures that `a` will error if not completed within `ms` milliseconds.
     */
    case "~%" => env.timeoutTASK
    /*
    @s (a >TASK)' -> TASK[ARR[NUM _]]'
    Times the execution of `a`, yielding the execution time (in ms) and the
    result.
     */
    case "~%Q" => env.timeTASK
    /*
    @s (a >TASK)' (f: ERR -> >TASK)' (g: x -> >TASK)' -> TASK'
    If `a` errors, then `f` is called with the error, providing an opportunity
    to recover.
    Otherwise, `g` is called with the result.
     */
    case "~?" => env.redeemTASK
    /*
    @s (a OBS) (f: x -> >TF)' -> OBS'
    @s (a >TASK)' (f: x -> >TF)' -> TASK'
    Continually retries `a` until `f` is truthy.
     */
    case "~@" => env.restartwTASK
    /*
    @s (ms >NUM)' -> OBS'
    Creates an `OBS` that emits whole numbers at a fixed rate of `ms`
    milliseconds.
     */
    case "~%@" => env.ointervalr
    /*
    @s (ms >NUM)' -> OBS'
    Creates an `OBS` that emits whole numbers with delays of `ms` milliseconds.
     */
    case "~+%@" => env.ointervald
    /*
    @s (a >TASK[x])' -> TASK[TRY[x]]'
    Wraps the contents of `a` in a `TRY`.
     */
    case "~!?" => env.wrapTRYTASK
    /*
    @s (a >TASK[TRY[x]])' -> TASK[x]'
    Unwraps a `TRY` inside `a`.
     */
    case "~!?_" => env.unwrapTRYTASK
    /*
    @s (a OBS) (f: x -> >OBS)' (g: y (e (TF | ERR)) -> >TASK)' -> OBS'
    @s (a >TASK) (f: x -> >TASK)' (g: y (e (TF | ERR)) -> >TASK)' -> TASK'
    Defines usage handler `f` and finalizer `g` for `a`.
    Suited for safely using and cleaning up resources.

    `e` is:
    - `$T` on completion.
    - `ERR` on error.
    - `$F` on cancellation.
     */
    case "~?[]" => env.bracketTASK
    /*
    @s (a OBS) (f: (e ERR) -> >TASK)' -> OBS'
    @s (a >TASK) (f: (e ERR) -> >TASK)' -> TASK'
    Transforms `f` when `a` errors.
     */
    case "~?!>" => env.onErrTASK
    /*
    @s (ms >NUM)' -> TASK[ms]'
    Creates an asynchronous `TASK` that completes after `ms` milliseconds.
     */
    case "~sleep" => env.sleepTASK
    /*
    @s (a OBS) (ms >NUM)' -> OBS'
    @s (a >TASK) (n >NUM)' -> TASK'
    Delays the `TASK` for `ms` milliseconds.
     */
    case "~delay" => env.delayTASK
    /*
    @s (a >OBS) (b >OBS) -> OBS
    Delays `a` until `b` either emits or completes.
     */
    case "~delay`" => env.odelay

    /*
    @s (a >OBS) (n >NUM)' -> OBS'
    Buffers up to `n` elements when downstream is busy.
    Back-pressures when buffer reaches `n`.
     */
    case "~/n" => env.obufferN
    /*
    @s (a >OBS) (ms >NUM)' -> OBS'
    Buffers elements within `ms`-millisecond timespans.
     */
    case "~/%" => env.obufferT
    /*
    @s (a >OBS) (ms >NUM)' (n >NUM)' -> OBS'
    Combines #{~/%} and #{~/n}.
    Force-emits instead of back-pressuring when buffer reaches `n`.
     */
    case "~/%n" => env.obufferTN
    /*
    @s (a >OBS) (ms >NUM)' (n >NUM)' (f: x -> >NUM)' -> OBS'
    #{~/%n} but with back-pressure instead of force-emitting.
    Also uses `f` to determine weight of each element.
     */
    case "~/%<" => env.obufferTB
    /*
    @s (a >OBS) (b >OBS) (n >NUM)' -> OBS'
    Buffers `a` until `b` emits.
    Back-pressures when buffer reaches `n`.
     */
    case "~/n`" => env.obufferON
    /*
    @s (a >OBS) (ms >NUM)' (n >NUM)' -> OBS[ARR]'
    Limits `a` to `n` emissions every `ms` milliseconds.
     */
    case "~>-<" => env.othrottle
    /*
    @s (a >OBS) (ms >NUM)' -> OBS[ARR]'
    Keeps the first emission of `a` per `ms`-millisecond timespan.
     */
    case "~>-^" => env.othrottleFirst
    /*
    @s (a >OBS) (ms >NUM)' -> OBS[ARR]'
    Keeps the last emission of `a` per `ms`-millisecond timespan.
     */
    case "~>-$" => env.othrottleLast
    /*
    @s (a >OBS) (ms >NUM)' -> OBS'
    Debounces `a` such that emissions only occur after `ms` milliseconds without
    emission.
     */
    case "~>~-" => env.odebounce

    /*
    @s -> OSTRAT
    Specifies that the buffer is unbounded. May exhaust system memory with fast
    data sources.
     */
    case "~^UN" => env.ostratUn
    /*
    @s (n >NUM)' -> OSTRAT
    Specifies that when the buffer reaches `n`, subscription is canceled with an
    error.
     */
    case "~^ERR" => env.ostratFail
    /*
    @s (n >NUM)' -> OSTRAT
    Specifies that when the buffer reaches `n`, back-pressure is applied.
     */
    case "~^BAK" => env.ostratBack
    /*
    @s (n >NUM)' -> OSTRAT
    Specifies that when the buffer reaches `n`, new elements are dropped.
     */
    case "~^NEW" => env.ostratNew
    /*
    @s (n >NUM)' -> OSTRAT
    Specifies that when the buffer reaches `n`, old elements are dropped.
     */
    case "~^OLD" => env.ostratOld
    /*
    @s (n >NUM)' -> OSTRAT
    Specifies that when the buffer reaches `n`, buffer is cleared.
     */
    case "~^CLR" => env.ostratClr

    /*
    @s a -> ARR
    Gets filepath `a` in segments.
     */
    case "_/<>" => env.pathARR
    /*
    @s a -> STR
    Gets filepath `a` as a string.
     */
    case "_/><" => env.pathSTR
    /*
    @s a -> STR
    Gets the filename of filepath `a`.
     */
    case "_/x" => env.pathname
    /*
    @s a -> STR
    Gets the basename of filepath `a`.
     */
    case "_/x_" => env.pathbase
    /*
    @s a -> STR
    Gets the file extension of filepath `a`.
     */
    case "_/_x" => env.pathext

    /*
    @s (a >STR) -> STR
    Converts `a` from UTF-8 to ISO-5589-1.
     */
    case "S>b" => env.utob
    /*
    @s (a >STR) -> STR
    Converts `a` from ISO-5589-1 to UTF-8.
     */
    case "b>S" => env.btou
    /*
    @s (a >STR) -> STR
    `OBS`-friendly #{S>b}.
     */
    case "~S>b" => env.oUtoB
    /*
    @s (a >STR) -> STR
    `OBS`-friendly #{b>S}.
     */
    case "~b>S" => env.oBtoU

    /*
    @s a -> OBS[STR*]
    Streams contents of filepath `a` as UTF-8.
     */
    case "fs>" => env.fsread
    /*
    @s a -> OBS[STR*]
    Streams contents of filepath `a` as bytes.
    If you know that `a` only contains codepoints 0-255,
    then this is a faster option than #{fs>}.
     */
    case "fs>b" => env.fsreadb
    /*
    @s a (b >STR) -> OBS[STR*]
    Streams lines of filepath `a` with encoding `b`.
    `b` defaults to UTF-8 when empty.
     */
    case "fs>n" => env.fsreadn
    /*
    @s (a >OBS[STR*]) b -> TASK[NUM]
    Streams UTF-8 `a` to filepath `b`.
     */
    case ">fs" => env.fswrite
    /*
    @s (a >OBS[STR*]) b -> TASK[NUM]
    Streams bytes `a` to filepath `b`.
     */
    case "b>fs" => env.fswriteb
    /*
    @s (a >OBS[STR*]) b (n >NUM)' -> TASK[NUM]'
    Streams UTF-8 `a` to filepath `b` starting at position `n`.
     */
    case "^>fs" => env.fswriteat
    /*
    @s (a >OBS[STR*]) b (n >NUM)' -> TASK[NUM]'
    Streams bytes `a` to filepath `b` starting at position `n`.
     */
    case "b^>fs" => env.fswriteatb

    /*
    @s a -> OBS[MAP["t"=>NUM "f"=>NUM "s"=>STR]]'
    Watches filepath `a` for changes.
    - `t` is the type; 1 for create, 2 for modify, and 3 for delete.
    - `n` is the count; greater than 1 signifies that the event is repeated.
    - `f` is the context, or the relative filepath that was changed.
     */
    case "fs@" => env.fswatch
    /*
    @s a -> OBS[STR*]
    Lists files at path `a`.
     */
    case "fs:" => env.fsls
    /*
    @s a -> OBS[STR*]
    Recursively lists files at path `a`.
     */
    case "fs::" => env.fslsr
    /*
    @s a (b >STR)' -> OBS[STR*]'
    Lists files at path `a` that match glob pattern `b`.
     */
    case "fs*" => env.fsglob
    /*
    @s a (b >STR)' -> OBS[STR*]'
    #{fs*} but with regex `b`.
     */
    case "fs*?" => env.fsglobR

    /*
    @s (a >STR)' (b >NUM)' -> OBS[STR*]'
    Streams host `a` and port `b` over TCP as UTF-8.
     */
    case "tcp>" => env.tcpread
    /*
    @s (a >STR)' (b >NUM)' -> OBS[STR*]'
    Streams host `a` and port `b` over TCP as bytes.
     */
    case "tcp>b" => env.tcpreadb

    /*
    @s (a >OBS[STR*]) (b >STR)' (c >NUM)' -> OBS[STR*]'
    Streams UTF-8 `a` over TCP to host `b` and port `c`.
     */
    case ">tcp" => env.tcpwrite
    /*
    @s (a >OBS[STR*]) (b >STR)' (c >NUM)' -> OBS[STR*]'
    Streams bytes `a` over TCP to host `b` and port `c`.
     */
    case "b>tcp" => env.tcpwriteb

    case _ => throw LinEx("FN", s"unknown fn \"$x\"")

    // CMDOC END
