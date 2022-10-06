
# COMMANDS


## ``` type ```

Stack: ``` a -> (STR x) ```

Pushes type of `a`.


## ``` form ```

Stack: ``` a -> (STR x) ```

Pushes `a` as formatted string.


## ``` >Q ```

Stack: ``` a -> (SEQ x) ```

Converts `a` to `SEQ`.


## ``` >A ```

Stack: ``` a -> (ARR x) ```

Converts `a` to `ARR`.


## ``` >M ```

Stack: ``` a -> (MAP x) ```

Converts `a` to `MAP`.


## ``` >S ```

Stack: ``` a -> (STR x) ```

Converts `a` to `STR`.


## ``` >N ```

Stack: ``` a -> (NUM x) ```

Converts `a` to `NUM`.


## ``` >F ```

Stack: ``` a -> (FN x) ```

Converts `a` to `FN`.


## ``` >E ```

Stack: ``` (a >STR) (b >STR) -> (ERR x) ```

Converts `a` to `ERR` with message `b`.


## ``` >? ```

Stack: ``` a -> (NUM x) ```

Pushes 1 or 0 depending on truthiness of `a`.


## ``` UN ```

Stack: ``` -> UN ```

Pushes `UN`.


## ``` () ```

Stack: ``` -> (FN x) ```

Pushes empty `FN`.


## ``` [] ```

Stack: ``` -> (ARR x) ```

Pushes empty `ARR`.


## ``` {} ```

Stack: ``` -> (MAP x) ```

Pushes empty `MAP`.


## ``` $PI ```

Stack: ``` -> (NUM x) ```

Pushes π (Pi).


## ``` $E ```

Stack: ``` -> (NUM x) ```

Pushes e (Euler's number).


## ``` $PHI ```

Stack: ``` -> (NUM x) ```

Pushes Φ (Golden Ratio).


## ``` $rng ```

Stack: ``` -> (NUM x) ```

Pushes uniformly random number.


## ``` $L ```

Stack: ``` -> (NUM x) ```

Pushes current line number of program execution.


## ``` $F ```

Stack: ``` -> (STR x) ```

Pushes current file of program execution.


## ``` $W ```

Stack: ``` -> (SEQ[NUM] x) ```

Pushes infinite list of 0 to ∞.


## ``` $N ```

Stack: ``` -> (SEQ[NUM] x) ```

Pushes infinite list of 1 to ∞.


## ``` $P ```

Stack: ``` -> (SEQ[NUM] x) ```

Pushes infinite list of primes.


## ``` i> ```

Stack: ``` -> (STR x) ```

Pushes line from STDIN.


## ``` >o ```

Stack: ``` (a >STR) -> ```

Sends `a` to STDOUT.


## ``` n>o ```

Stack: ``` (a >STR) -> ```

Sends `a` as line to STDOUT.


## ``` f>o ```

Stack: ``` a -> ```

`form`s and `n>o`s `a`.


## ``` dup ```

Stack: ``` a -> a a ```




## ``` dups ```

Stack: ``` a* -> a* (ARR a*) ```




## ``` over ```

Stack: ``` a b -> a b a ```




## ``` pick ```

Stack: ``` (a @ n) b* (n >NUM) -> a b* a ```

`dup`s `n`th item from top of stack.


## ``` pop ```

Stack: ``` _ -> ```




## ``` clr ```

Stack: ``` _* -> ```




## ``` nip ```

Stack: ``` _ b -> b ```




## ``` nix ```

Stack: ``` (a @ n) b* (n >NUM) -> b* ```

`pop`s `n`th item from top of stack.


## ``` swap ```

Stack: ``` a b -> b a ```




## ``` rev ```

Stack: ``` a* -> x* ```

Reverses stack.


## ``` tuck ```

Stack: ``` a b -> b a b ```




## ``` trade ```

Stack: ``` (a @ n) b* c (n >NUM) -> c b* a ```

`swaps`s `c` with `n`th item from top of stack.


## ``` rot ```

Stack: ``` a b c -> b c a ```




## ``` rot_ ```

Stack: ``` a b c -> c a b ```




## ``` roll ```

Stack: ``` (a @ n) b* (n >NUM) -> b* a ```

`rot`s to top `n`th item from top of stack.


## ``` roll_ ```

Stack: ``` b* c (n >NUM) -> (c @ n) b* ```

`rot_`s `c` to `n`th from top of stack.


## ``` dip ```

Stack: ``` a* b (f >FN) -> x* b ```

`pop`s `b`, executes `f`, and pushes `b`.


## ``` \ ```

Stack: ``` a -> (FN a) ```

Wraps `a` in `FN`.


## ``` # ```

Stack: ``` a* f -> x* ```

Executes `f`.


## ``` Q ```

Stack: ``` f -> y ```

Evaluates `f` (`#` but only preserves resulting top of stack).


## ``` @@ ```

Stack: ``` a* (n >NUM) -> x* ```

Executes `n`th line.


## ``` @~ ```

Stack: ``` a* (n >NUM) -> x* ```

Executes `n`th line relative to current line.


## ``` @ ```

Stack: ``` a* -> x* ```

Executes current line.


## ``` ; ```

Stack: ``` a* -> x* ```

Executes next line.


## ``` ;; ```

Stack: ``` a* -> x* ```

Executes previous line.

