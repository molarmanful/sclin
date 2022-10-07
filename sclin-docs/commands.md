
# COMMANDS


## CMD: [``` type ```](#cmd-type)

Stack: ``` a -> (x STR) ```

Type of `a`.


## CMD: [``` form ```](#cmd-form)

Stack: ``` a -> (x STR) ```

`a` as formatted string.


## CMD: [``` >Q ```](#cmd-q)

Stack: ``` a -> (x STR) ```

Converts `a` to `SEQ`.


## CMD: [``` >A ```](#cmd-a)

Stack: ``` a -> (x ARR) ```

Converts `a` to `ARR`.


## CMD: [``` >M ```](#cmd-m)

Stack: ``` a -> (x ARR) ```

Converts `a` to `MAP`.


## CMD: [``` >S ```](#cmd-s)

Stack: ``` a -> (x STR) ```

Converts `a` to `STR`.


## CMD: [``` >N ```](#cmd-n)

Stack: ``` a -> (x NUM) ```

Converts `a` to `NUM`.


## CMD: [``` >F ```](#cmd-f)

Stack: ``` a -> (x FN) ```

Converts `a` to `FN`.


## CMD: [``` >E ```](#cmd-e)

Stack: ``` (a >STR) (b >STR) -> (x ERR) ```

Converts `a` to `ERR` with message `b`.


## CMD: [``` >? ```](#cmd-)

Stack: ``` a -> (x NUM) ```

1 or 0 depending on truthiness of `a`.


## CMD: [``` UN ```](#cmd-un)

Stack: ``` -> UN ```

`UN`


## CMD: [``` () ```](#cmd--1)

Stack: ``` -> (x FN) ```

Empty `FN`.


## CMD: [``` [] ```](#cmd--2)

Stack: ``` -> (x ARR) ```

Empty `ARR`.


## CMD: [``` {} ```](#cmd--3)

Stack: ``` -> (x MAP) ```

Empty `MAP`.


## CMD: [``` $PI ```](#cmd-pi)

Stack: ``` -> (x NUM) ```

π (Pi).


## CMD: [``` $E ```](#cmd-e-1)

Stack: ``` -> (x NUM) ```

e (Euler's number).


## CMD: [``` $PHI ```](#cmd-phi)

Stack: ``` -> (x NUM) ```

Φ (Golden Ratio).


## CMD: [``` $rng ```](#cmd-rng)

Stack: ``` -> (x NUM) ```

Uniformly random number.


## CMD: [``` $L ```](#cmd-l)

Stack: ``` -> (x NUM) ```

Current line number of program execution.


## CMD: [``` $F ```](#cmd-f-1)

Stack: ``` -> (x STR) ```

Current file of program execution.


## CMD: [``` $W ```](#cmd-w)

Stack: ``` -> (x SEQ[(y NUM)*]) ```

Infinite list of 0 to ∞.


## CMD: [``` $N ```](#cmd-n-1)

Stack: ``` -> (x SEQ[(y NUM)*]) ```

Infinite list of 1 to ∞.


## CMD: [``` $P ```](#cmd-p)

Stack: ``` -> (x SEQ[(y NUM)*]) ```

Infinite list of primes.


## CMD: [``` g@ ```](#cmd-g)

Stack: ``` -> (x STR) | UN ```

Current line.


## CMD: [``` g; ```](#cmd-g-1)

Stack: ``` -> (x STR) | UN ```

Next line.


## CMD: [``` g;; ```](#cmd-g-2)

Stack: ``` -> (x STR) | UN ```

Previous line.


## CMD: [``` i> ```](#cmd-i)

Stack: ``` -> (x STR) ```

Line from STDIN.


## CMD: [``` >o ```](#cmd-o)

Stack: ``` (a >STR) -> ```

Sends `a` to STDOUT.


## CMD: [``` n>o ```](#cmd-no)

Stack: ``` (a >STR) -> ```

[``` >o ```](#cmd-o)s `a` with trailing newline.


## CMD: [``` f>o ```](#cmd-fo)

Stack: ``` a -> ```

[``` form ```](#cmd-form)s and [``` n>o ```](#cmd-no)s `a`.


## CMD: [``` dup ```](#cmd-dup)

Stack: ``` a -> a a ```




## CMD: [``` dups ```](#cmd-dups)

Stack: ``` a* -> a* (x ARR[a*]) ```




## CMD: [``` over ```](#cmd-over)

Stack: ``` a b -> a b a ```




## CMD: [``` pick ```](#cmd-pick)

Stack: ``` (a @ n) b* (n >NUM) -> a b* a ```

[``` dup ```](#cmd-dup)s `n`th item from top of stack.


## CMD: [``` pop ```](#cmd-pop)

Stack: ``` _ -> ```




## CMD: [``` clr ```](#cmd-clr)

Stack: ``` _* -> ```




## CMD: [``` nip ```](#cmd-nip)

Stack: ``` _ b -> b ```




## CMD: [``` nix ```](#cmd-nix)

Stack: ``` (a @ n) b* (n >NUM) -> b* ```

[``` pop ```](#cmd-pop)s `n`th item from top of stack.


## CMD: [``` swap ```](#cmd-swap)

Stack: ``` a b -> b a ```




## CMD: [``` rev ```](#cmd-rev)

Stack: ``` a* -> x* ```

Reverses stack.


## CMD: [``` tuck ```](#cmd-tuck)

Stack: ``` a b -> b a b ```




## CMD: [``` trade ```](#cmd-trade)

Stack: ``` (a @ n) b* c (n >NUM) -> c b* a ```

[``` swap ```](#cmd-swap)s `c` with `n`th item from top of stack.


## CMD: [``` rot ```](#cmd-rot)

Stack: ``` a b c -> b c a ```




## CMD: [``` rot_ ```](#cmd-rot_)

Stack: ``` a b c -> c a b ```




## CMD: [``` roll ```](#cmd-roll)

Stack: ``` (a @ n) b* (n >NUM) -> b* a ```

[``` rot ```](#cmd-rot)s to top `n`th item from top of stack.


## CMD: [``` roll_ ```](#cmd-roll_)

Stack: ``` b* c (n >NUM) -> (c @ n) b* ```

[``` rot_ ```](#cmd-rot_)s `c` to `n`th from top of stack.


## CMD: [``` dip ```](#cmd-dip)

Stack: ``` a* b (f >FN) -> x* b ```

[``` pop ```](#cmd-pop)s `b`, executes `f`, and pushes `b`.


## CMD: [``` \ ```](#cmd--4)

Stack: ``` a -> (a FN) ```

Wraps `a` in `FN`.


## CMD: [``` # ```](#cmd--5)

Stack: ``` a* f -> x* ```

Executes `f`.


## CMD: [``` Q ```](#cmd-q-1)

Stack: ``` f' -> x' ```

Evaluates `f` ([``` # ```](#cmd--5) but only preserves resulting top of stack).


## CMD: [``` @@ ```](#cmd--6)

Stack: ``` a* (n >NUM) -> x* ```

[``` # ```](#cmd--5)s `n`th line.


## CMD: [``` @~ ```](#cmd--7)

Stack: ``` a* (n >NUM) -> x* ```

[``` # ```](#cmd--5)s `n`th line relative to current line.


## CMD: [``` @ ```](#cmd--8)

Stack: ``` a* -> x* ```

[``` # ```](#cmd--5)s current line.


## CMD: [``` ; ```](#cmd--9)

Stack: ``` a* -> x* ```

[``` # ```](#cmd--5)s next line.


## CMD: [``` ;; ```](#cmd--10)

Stack: ``` a* -> x* ```

[``` # ```](#cmd--5)s previous line.


## CMD: [``` g@@ ```](#cmd-g-3)

Stack: ``` (n >NUM) -> (x STR) | UN ```

`n`th line.


## CMD: [``` g@~ ```](#cmd-g-4)

Stack: ``` (n >NUM) -> (x STR) | UN ```

`n`th line relative to current line.


## CMD: [``` &# ```](#cmd--11)

Stack: ``` a* b f -> x* ```

[``` # ```](#cmd--5)s `f` if `b` is truthy.


## CMD: [``` |# ```](#cmd--12)

Stack: ``` a* b f -> x* ```

[``` # ```](#cmd--5)s `f` if `b` is falsy.


## CMD: [``` ?# ```](#cmd--13)

Stack: ``` a* b f g -> x* ```

[``` # ```](#cmd--5)s `f` if `b` is truthy; else [``` # ```](#cmd--5)s `g`.


## CMD: [``` *# ```](#cmd--14)

Stack: ``` a* f (n >NUM) -> x* ```

[``` # ```](#cmd--5)s `f` `n` times.


## CMD: [``` !# ```](#cmd--15)

Stack: ``` a* f g -> x* ```

Tries to [``` # ```](#cmd--5) `f`; on error, pushes caught `ERR` and [``` # ```](#cmd--5)s `g`.


## CMD: [``` >! ```](#cmd--16)

Stack: ``` (e ERR) -> ```

Throws `e`.


## CMD: [``` ' ```](#cmd--17)

Stack: ``` (a >ARR) f -> (x ARR) ```

[``` # ```](#cmd--5)s `f` on `a` as if it were a stack.


## CMD: [``` '_ ```](#cmd-_)

Stack: ``` (a* >ARR) f -> x* ```

[``` # ```](#cmd--5)s `f` on the stack as if it were an `ARR`.


## CMD: [``` E ```](#cmd-e-2)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

`a * 10 ^ b`


## CMD: [``` I ```](#cmd-i-1)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Rounds `a` towards 0.


## CMD: [``` |_ ```](#cmd-_-1)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Rounds `a` towards -∞.


## CMD: [``` |~ ```](#cmd--18)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Rounds `a` to nearest integer.


## CMD: [``` |^ ```](#cmd--19)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Rounds `a` towards ∞.


## CMD: [``` _ ```](#cmd-_-2)

Stack: ``` (a >NUM)' -> (x NUM)' ```

`-a`


## CMD: [``` __ ```](#cmd-__)

Stack: ``` (a >STR)' -> (x STR)' ```

Atom-reverses `a`.


## CMD: [``` _` ```](#cmd-_-3)

Stack: ``` a -> x ```

Reverses `a`.


## CMD: [``` + ```](#cmd--20)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

`a + b`


## CMD: [``` ++ ```](#cmd--21)

Stack: ``` (a >STR)' (b >STR)' -> (x STR)' ```

Atomic [``` +` ```](#cmd--22).


## CMD: [``` +` ```](#cmd--22)

Stack: ``` a b -> x ```

Concatenates `a` and `b`.


## CMD: [``` - ```](#cmd--)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

`a - b`


## CMD: [``` * ```](#cmd--23)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

`a * b`


## CMD: [``` / ```](#cmd--24)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

`a / b`. Throws error if `b` is 0.


## CMD: [``` /~ ```](#cmd--25)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

Integer [``` / ```](#cmd--24).


## CMD: [``` % ```](#cmd--26)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

`a (mod b)`


## CMD: [``` /% ```](#cmd--27)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' (y NUM)' ```

Results of [``` /~ ```](#cmd--25) and [``` % ```](#cmd--26) on `a` and `b`.


## CMD: [``` ^ ```](#cmd--28)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

`a ^ b`. Throws error if result would be a complex number.


## CMD: [``` ^~ ```](#cmd--29)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

[``` ^ ```](#cmd--28) but `b` is coerced to `int`.


## CMD: [``` e^ ```](#cmd-e-3)

Stack: ``` (a >NUM)' -> (x NUM)' ```

`e ^ a`


## CMD: [``` abs ```](#cmd-abs)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Absolute value of `a`.


## CMD: [``` sin ```](#cmd-sin)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Sine of `a`.


## CMD: [``` cos ```](#cmd-cos)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Cosine of `a`.


## CMD: [``` tan ```](#cmd-tan)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Tangent of `a`.


## CMD: [``` sin_ ```](#cmd-sin_)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Arcsine of `a`.


## CMD: [``` cos_ ```](#cmd-cos_)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Arccosine of `a`.


## CMD: [``` tan_ ```](#cmd-tan_)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Arctangent of `a`.


## CMD: [``` tan_II ```](#cmd-tan_ii)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

Arctangent of `a` with `b` as quadrant.


## CMD: [``` sinh ```](#cmd-sinh)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Hyperbolic sine of `a`.


## CMD: [``` cosh ```](#cmd-cosh)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Hyperbolic cosine of `a`.


## CMD: [``` tanh ```](#cmd-tanh)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Hyperbolic tangent of `a`.


## CMD: [``` sinh_ ```](#cmd-sinh_)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Hyperbolic arcsine of `a`.


## CMD: [``` cosh_ ```](#cmd-cosh_)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Hyperbolic arccosine of `a`.


## CMD: [``` tanh_ ```](#cmd-tanh_)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Hyperbolic arctangent of `a`.


## CMD: [``` log ```](#cmd-log)

Stack: ``` (a >NUM)' (b >NUM)' -> (x NUM)' ```

Base `b` logarithm of `a`.


## CMD: [``` ln ```](#cmd-ln)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Natural logarithm of `a`.


## CMD: [``` logX ```](#cmd-logx)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Base-10 logarithm of `a`.


## CMD: [``` P? ```](#cmd-p-1)

Stack: ``` (a >NUM)' -> (x NUM)' ```

Whether `a` is prime. Uses a strong pseudo-primality test with a 1/1e12 chance of being wrong.


## CMD: [``` P/ ```](#cmd-p-2)

Stack: ``` (a >NUM)' -> (x MAP[((y NUM) => (z NUM))*])' ```

Prime-factorizes `a` into pairs of prime `y` and frequency `z`.


## CMD: [``` ! ```](#cmd--30)

Stack: ``` a' -> (x NUM)' ```

Atomic [``` !` ```](#cmd--31).


## CMD: [``` !` ```](#cmd--31)

Stack: ``` a -> (x NUM) ```

Logical NOT.


## CMD: [``` & ```](#cmd--32)

Stack: ``` a' b' -> (a | b)' ```

Atomic [``` &` ```](#cmd--34).


## CMD: [``` && ```](#cmd--33)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` &&` ```](#cmd--35).


## CMD: [``` &` ```](#cmd--34)

Stack: ``` a b -> a | b ```

Minimum of `a` and `b`.


## CMD: [``` &&` ```](#cmd--35)

Stack: ``` a b -> (x NUM) ```

Logical AND of `a` and `b`.


## CMD: [``` | ```](#cmd--36)

Stack: ``` a' b' -> (a | b)' ```

Atomic [``` |` ```](#cmd--38).


## CMD: [``` || ```](#cmd--37)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` ||` ```](#cmd--39).


## CMD: [``` |` ```](#cmd--38)

Stack: ``` a b -> a | b ```

Maximum of `a` and `b`.


## CMD: [``` ||` ```](#cmd--39)

Stack: ``` a b -> (x NUM) ```

Logical OR of `a` and `b`.


## CMD: [``` <=> ```](#cmd--40)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` <=>` ```](#cmd--41).


## CMD: [``` <=>` ```](#cmd--41)

Stack: ``` a b -> x ```

Comparison (-1, 0, or 1 depending on whether `a` is less than, equal to, or greater than `b`).


## CMD: [``` = ```](#cmd--42)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` =` ```](#cmd--43).


## CMD: [``` =` ```](#cmd--43)

Stack: ``` a b -> (x NUM) ```

Whether `a` equals `b`.


## CMD: [``` != ```](#cmd--44)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` !=` ```](#cmd--45).


## CMD: [``` !=` ```](#cmd--45)

Stack: ``` a b -> (x NUM) ```

Whether `a` does not equals `b`.


## CMD: [``` < ```](#cmd--46)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` <` ```](#cmd--47).


## CMD: [``` <` ```](#cmd--47)

Stack: ``` a b -> (x NUM) ```

Whether `a` is less than `b`.


## CMD: [``` > ```](#cmd--48)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` >` ```](#cmd--49).


## CMD: [``` >` ```](#cmd--49)

Stack: ``` a b -> (x NUM) ```

Whether `a` is greater than `b`.


## CMD: [``` <= ```](#cmd--50)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` <=` ```](#cmd--51).


## CMD: [``` <=` ```](#cmd--51)

Stack: ``` a b -> (x NUM) ```

Whether `a` is less than or equal to `b`.


## CMD: [``` >= ```](#cmd--52)

Stack: ``` a' b' -> (x NUM)' ```

Atomic [``` >=` ```](#cmd--53).


## CMD: [``` >=` ```](#cmd--53)

Stack: ``` a b -> (x NUM) ```

Whether `a` is greater than or equal to `b`.


## CMD: [``` : ```](#cmd--54)

Stack: ``` a i' -> (a.x | UN)' ```

Value at atomic index `i` in `a`.


## CMD: [``` :r ```](#cmd-r)

Stack: ``` a -> a.x ```

Value at random index in `a`.


## CMD: [``` :` ```](#cmd--55)

Stack: ``` a i -> a.x | UN ```

Value at index `i` in `a`.


## CMD: [``` :? ```](#cmd--56)

Stack: ``` a b' -> (x NUM)' ```

Whether `a` has atomic `b`.


## CMD: [``` :?` ```](#cmd--57)

Stack: ``` a b -> (x NUM) ```

Whether `a` has `b`. `MAP`s check `b` against keys; other types of `a` check `b` against values.


## CMD: [``` len ```](#cmd-len)

Stack: ``` a -> (x NUM) ```

Length of `a`.


## CMD: [``` , ```](#cmd--58)

Stack: ``` a b -> (x ARR[a b]) ```

Pairs `a` and `b` in an `ARR`.


## CMD: [``` ,, ```](#cmd--59)

Stack: ``` a -> (x ARR[a]) ```

Wraps `a` in an `ARR`.


## CMD: [``` ,` ```](#cmd--60)

Stack: ``` a* -> a ```

Wraps stack in an `ARR`.


## CMD: [``` ,_ ```](#cmd-_-4)

Stack: ``` a -> a* ```

Unwraps `a`.


## CMD: [``` ,,_ ```](#cmd-_-5)

Stack: ``` _* a -> a* ```

Replaces stack with `a` unwrapped.

