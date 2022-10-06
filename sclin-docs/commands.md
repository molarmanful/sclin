
# COMMANDS


## CMD: ``` type ```

Stack: ``` a -> (x STR) ```

Pushes type of `a`.


## CMD: ``` form ```

Stack: ``` a -> (x STR) ```

Pushes `a` as formatted string.


## CMD: ``` >Q ```

Stack: ``` a -> (x STR) ```

Converts `a` to `SEQ`.


## CMD: ``` >A ```

Stack: ``` a -> (x ARR) ```

Converts `a` to `ARR`.


## CMD: ``` >M ```

Stack: ``` a -> (x ARR) ```

Converts `a` to `MAP`.


## CMD: ``` >S ```

Stack: ``` a -> (x STR) ```

Converts `a` to `STR`.


## CMD: ``` >N ```

Stack: ``` a -> (x NUM) ```

Converts `a` to `NUM`.


## CMD: ``` >F ```

Stack: ``` a -> (x FN) ```

Converts `a` to `FN`.


## CMD: ``` >E ```

Stack: ``` (a >STR) (b >STR) -> (x ERR) ```

Converts `a` to `ERR` with message `b`.


## CMD: ``` >? ```

Stack: ``` a -> (x NUM) ```

Pushes 1 or 0 depending on truthiness of `a`.


## CMD: ``` UN ```

Stack: ``` -> UN ```

Pushes `UN`.


## CMD: ``` () ```

Stack: ``` -> (x FN) ```

Pushes empty `FN`.


## CMD: ``` [] ```

Stack: ``` -> (x ARR) ```

Pushes empty `ARR`.


## CMD: ``` {} ```

Stack: ``` -> (x MAP) ```

Pushes empty `MAP`.


## CMD: ``` $PI ```

Stack: ``` -> (x NUM) ```

Pushes π (Pi).


## CMD: ``` $E ```

Stack: ``` -> (x NUM) ```

Pushes e (Euler's number).


## CMD: ``` $PHI ```

Stack: ``` -> (x NUM) ```

Pushes Φ (Golden Ratio).


## CMD: ``` $rng ```

Stack: ``` -> (x NUM) ```

Pushes uniformly random number.


## CMD: ``` $L ```

Stack: ``` -> (x NUM) ```

Pushes current line number of program execution.


## CMD: ``` $F ```

Stack: ``` -> (x STR) ```

Pushes current file of program execution.


## CMD: ``` $W ```

Stack: ``` -> (x SEQ[NUM]) ```

Pushes infinite list of 0 to ∞.


## CMD: ``` $N ```

Stack: ``` -> (x SEQ[NUM]) ```

Pushes infinite list of 1 to ∞.


## CMD: ``` $P ```

Stack: ``` -> (x SEQ[NUM]) ```

Pushes infinite list of primes.


## CMD: ``` g@ ```

Stack: ``` -> (x STR) ```

Pushes current line.


## CMD: ``` g; ```

Stack: ``` -> (x STR) ```

Pushes next line.


## CMD: ``` g;; ```

Stack: ``` -> (x STR) ```

Pushes previous line.


## CMD: ``` i> ```

Stack: ``` -> (x STR) ```

Pushes line from STDIN.


## CMD: ``` >o ```

Stack: ``` (a >STR) -> ```

Sends `a` to STDOUT.


## CMD: ``` n>o ```

Stack: ``` (a >STR) -> ```

[``` >o ```](#cmd-o)s `a` with trailing newline.


## CMD: ``` f>o ```

Stack: ``` a -> ```

[``` form ```](#cmd-form)s and [``` n>o ```](#cmd-no)s `a`.


## CMD: ``` dup ```

Stack: ``` a -> a a ```




## CMD: ``` dups ```

Stack: ``` a* -> a* (a* ARR) ```




## CMD: ``` over ```

Stack: ``` a b -> a b a ```




## CMD: ``` pick ```

Stack: ``` (a @ n) b* (n >NUM) -> a b* a ```

[``` dup ```](#cmd-dup)s `n`th item from top of stack.


## CMD: ``` pop ```

Stack: ``` _ -> ```




## CMD: ``` clr ```

Stack: ``` _* -> ```




## CMD: ``` nip ```

Stack: ``` _ b -> b ```




## CMD: ``` nix ```

Stack: ``` (a @ n) b* (n >NUM) -> b* ```

[``` pop ```](#cmd-pop)s `n`th item from top of stack.


## CMD: ``` swap ```

Stack: ``` a b -> b a ```




## CMD: ``` rev ```

Stack: ``` a* -> x* ```

Reverses stack.


## CMD: ``` tuck ```

Stack: ``` a b -> b a b ```




## CMD: ``` trade ```

Stack: ``` (a @ n) b* c (n >NUM) -> c b* a ```

[``` swap ```](#cmd-swap)s `c` with `n`th item from top of stack.


## CMD: ``` rot ```

Stack: ``` a b c -> b c a ```




## CMD: ``` rot_ ```

Stack: ``` a b c -> c a b ```




## CMD: ``` roll ```

Stack: ``` (a @ n) b* (n >NUM) -> b* a ```

[``` rot ```](#cmd-rot)s to top `n`th item from top of stack.


## CMD: ``` roll_ ```

Stack: ``` b* c (n >NUM) -> (c @ n) b* ```

[``` rot_ ```](#cmd-rot_)s `c` to `n`th from top of stack.


## CMD: ``` dip ```

Stack: ``` a* b (f >FN) -> x* b ```

[``` pop ```](#cmd-pop)s `b`, executes `f`, and pushes `b`.


## CMD: ``` \ ```

Stack: ``` a -> (a FN) ```

Wraps `a` in `FN`.


## CMD: ``` # ```

Stack: ``` a* f -> x* ```

Executes `f`.


## CMD: ``` Q ```

Stack: ``` f -> y ```

Evaluates `f` ([``` # ```](#cmd--5) but only preserves resulting top of stack).


## CMD: ``` @@ ```

Stack: ``` a* (n >NUM) -> x* ```

[``` # ```](#cmd--5)s `n`th line.


## CMD: ``` @~ ```

Stack: ``` a* (n >NUM) -> x* ```

[``` # ```](#cmd--5)s `n`th line relative to current line.


## CMD: ``` @ ```

Stack: ``` a* -> x* ```

[``` # ```](#cmd--5)s current line.


## CMD: ``` ; ```

Stack: ``` a* -> x* ```

[``` # ```](#cmd--5)s next line.


## CMD: ``` ;; ```

Stack: ``` a* -> x* ```

[``` # ```](#cmd--5)s previous line.


## CMD: ``` g@@ ```

Stack: ``` (n >NUM) -> (x STR) ```

Pushes `n`th line.


## CMD: ``` g@~ ```

Stack: ``` (n >NUM) -> (x STR) ```

Pushes `n`th line relative to current line.


## CMD: ``` &# ```

Stack: ``` a* b f -> x* ```

[``` # ```](#cmd--5)s `f` if `b` is truthy.

