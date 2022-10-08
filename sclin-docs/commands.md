
# COMMANDS


## CMD: [``` type ```](#cmd-type)

Stack: ``` a -> STR ```

Type of `a`.


## CMD: [``` form ```](#cmd-form)

Stack: ``` a -> STR ```

`a` as formatted string.


## CMD: [``` >Q ```](#cmd-q)

Stack: ``` a -> STR ```

Converts `a` to `SEQ`.


## CMD: [``` >A ```](#cmd-a)

Stack: ``` a -> ARR ```

Converts `a` to `ARR`.


## CMD: [``` >M ```](#cmd-m)

Stack: ``` a -> ARR ```

Converts `a` to `MAP`.


## CMD: [``` >S ```](#cmd-s)

Stack: ``` a -> STR ```

Converts `a` to `STR`.


## CMD: [``` >N ```](#cmd-n)

Stack: ``` a -> NUM ```

Converts `a` to `NUM`.


## CMD: [``` >F ```](#cmd-f)

Stack: ``` a -> FN ```

Converts `a` to `FN`.


## CMD: [``` >E ```](#cmd-e)

Stack: ``` (a >STR) (b >STR) -> ERR ```

Converts `a` to `ERR` with message `b`.


## CMD: [``` >? ```](#cmd-)

Stack: ``` a -> NUM ```

1 or 0 depending on truthiness of `a`.


## CMD: [``` UN ```](#cmd-un)

Stack: ``` -> UN ```

`UN`


## CMD: [``` () ```](#cmd--1)

Stack: ``` -> FN ```

Empty `FN`.


## CMD: [``` [] ```](#cmd--2)

Stack: ``` -> ARR ```

Empty `ARR`.


## CMD: [``` {} ```](#cmd--3)

Stack: ``` -> MAP ```

Empty `MAP`.


## CMD: [``` $PI ```](#cmd-pi)

Stack: ``` -> NUM ```

π (Pi).


## CMD: [``` $E ```](#cmd-e-1)

Stack: ``` -> NUM ```

e (Euler's number).


## CMD: [``` $PHI ```](#cmd-phi)

Stack: ``` -> NUM ```

Φ (Golden Ratio).


## CMD: [``` $rng ```](#cmd-rng)

Stack: ``` -> NUM ```

Uniformly random number.


## CMD: [``` $L ```](#cmd-l)

Stack: ``` -> NUM ```

Current line number of program execution.


## CMD: [``` $F ```](#cmd-f-1)

Stack: ``` -> STR ```

Current file of program execution.


## CMD: [``` $W ```](#cmd-w)

Stack: ``` -> SEQ[NUM*] ```

Infinite `SEQ` of 0 to ∞.


## CMD: [``` $N ```](#cmd-n-1)

Stack: ``` -> SEQ[NUM*] ```

Infinite `SEQ` of 1 to ∞.


## CMD: [``` $P ```](#cmd-p)

Stack: ``` -> SEQ[NUM*] ```

Infinite `SEQ` of primes.


## CMD: [``` g@ ```](#cmd-g)

Stack: ``` -> STR | UN ```

Current line.


## CMD: [``` g; ```](#cmd-g-1)

Stack: ``` -> STR | UN ```

Next line.


## CMD: [``` g;; ```](#cmd-g-2)

Stack: ``` -> STR | UN ```

Previous line.


## CMD: [``` i> ```](#cmd-i)

Stack: ``` -> STR ```

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

Stack: ``` a* -> a* ARR[a*] ```




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

Stack: ``` (a @ n) b* (n >NUM) -> _* ```

[``` pop ```](#cmd-pop)s `n`th item from top of stack.


## CMD: [``` swap ```](#cmd-swap)

Stack: ``` a b -> b a ```




## CMD: [``` rev ```](#cmd-rev)

Stack: ``` a* -> _* ```

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

Stack: ``` a* b (f >FN) -> _* b ```

[``` pop ```](#cmd-pop)s `b`, executes `f`, and pushes `b`.


## CMD: [``` \ ```](#cmd--4)

Stack: ``` a -> FN[a] ```

Wraps `a` in `FN`.


## CMD: [``` # ```](#cmd--5)

Stack: ``` a* f -> _* ```

Executes `f`.


## CMD: [``` Q ```](#cmd-q-1)

Stack: ``` f' -> _' ```

Evaluates `f` ([``` # ```](#cmd--5) but only preserves resulting top of stack).


## CMD: [``` @@ ```](#cmd--6)

Stack: ``` a* (n >NUM) -> _* ```

[``` # ```](#cmd--5)s `n`th line.


## CMD: [``` @~ ```](#cmd--7)

Stack: ``` a* (n >NUM) -> _* ```

[``` # ```](#cmd--5)s `n`th line relative to current line.


## CMD: [``` @ ```](#cmd--8)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--5)s current line.


## CMD: [``` ; ```](#cmd--9)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--5)s next line.


## CMD: [``` ;; ```](#cmd--10)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--5)s previous line.


## CMD: [``` g@@ ```](#cmd-g-3)

Stack: ``` (n >NUM) -> STR | UN ```

`n`th line.


## CMD: [``` g@~ ```](#cmd-g-4)

Stack: ``` (n >NUM) -> STR | UN ```

`n`th line relative to current line.


## CMD: [``` &# ```](#cmd--11)

Stack: ``` a* b f -> _* ```

[``` # ```](#cmd--5)s `f` if `b` is truthy.


## CMD: [``` |# ```](#cmd--12)

Stack: ``` a* b f -> _* ```

[``` # ```](#cmd--5)s `f` if `b` is falsy.


## CMD: [``` ?# ```](#cmd--13)

Stack: ``` a* b f g -> _* ```

[``` # ```](#cmd--5)s `f` if `b` is truthy; else [``` # ```](#cmd--5)s `g`.


## CMD: [``` *# ```](#cmd--14)

Stack: ``` a* f (n >NUM) -> _* ```

[``` # ```](#cmd--5)s `f` `n` times.


## CMD: [``` !# ```](#cmd--15)

Stack: ``` a* f g -> _* ```

Tries to [``` # ```](#cmd--5) `f`; on error, pushes caught `ERR` and [``` # ```](#cmd--5)s `g`.


## CMD: [``` >! ```](#cmd--16)

Stack: ``` (e ERR) -> ```

Throws `e`.


## CMD: [``` ' ```](#cmd--17)

Stack: ``` (a >ARR) f -> ARR ```

[``` # ```](#cmd--5)s `f` on `a` as if it were a stack.


## CMD: [``` '_ ```](#cmd-_)

Stack: ``` (a* >ARR) f -> _* ```

[``` # ```](#cmd--5)s `f` on the stack as if it were an `ARR`.


## CMD: [``` E ```](#cmd-e-2)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a * 10 ^ b`


## CMD: [``` I ```](#cmd-i-1)

Stack: ``` @s (a >NUM)' -> NUM' ```

Rounds `a` towards 0.


## CMD: [``` |_ ```](#cmd-_-1)

Stack: ``` @s (a >NUM)' -> NUM' ```

Rounds `a` towards -∞.


## CMD: [``` |~ ```](#cmd--18)

Stack: ``` @s (a >NUM)' -> NUM' ```

Rounds `a` to nearest integer.


## CMD: [``` |^ ```](#cmd--19)

Stack: ``` @s (a >NUM)' -> NUM' ```

Rounds `a` towards ∞.


## CMD: [``` _ ```](#cmd-_-2)

Stack: ``` @s (a >NUM)' -> NUM' ```

`-a`


## CMD: [``` __ ```](#cmd-__)

Stack: ``` (a >STR)' -> STR' ```

Atom-reverses `a`.


## CMD: [``` _` ```](#cmd-_-3)

Stack: ``` a -> _ ```

Reverses `a`.


## CMD: [``` + ```](#cmd--20)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a + b`


## CMD: [``` ++ ```](#cmd--21)

Stack: ``` (a >STR)' (b >STR)' -> STR' ```

Atomic [``` +` ```](#cmd--22).


## CMD: [``` +` ```](#cmd--22)

Stack: ``` a b -> _ ```

Concatenates `a` and `b`.


## CMD: [``` - ```](#cmd--)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a - b`


## CMD: [``` -- ```](#cmd---)

Stack: ``` (a >STR)' (b >STR)' -> STR' ```

Atomic [``` -` ```](#cmd---1).


## CMD: [``` -` ```](#cmd---1)

Stack: ``` a b -> _ ```

Remove occurrences of `b` from `a`.If `a` is `MAP`, then removal is performed on keys.


## CMD: [``` * ```](#cmd--23)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a * b`


## CMD: [``` ** ```](#cmd--24)

Stack: ``` (a >STR)' (b >NUM)' -> STR' ```

Atomic [``` *` ```](#cmd--25).


## CMD: [``` *` ```](#cmd--25)

Stack: ``` a b -> _ ```

`a` replicated according to `b`.If `b` is iterable, then `a` and `b` are recursively zipped together and replicated.


## CMD: [``` / ```](#cmd--26)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a / b`. Throws error if `b` is 0.


## CMD: [``` /~ ```](#cmd--27)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

Integer [``` / ```](#cmd--26).


## CMD: [``` // ```](#cmd--28)

Stack: ``` (a >STR)' (b >NUM)' -> ARR[STR]' ```

Atomic [``` /` ```](#cmd--29).


## CMD: [``` /` ```](#cmd--29)

Stack: ``` a (b >NUM)' -> SEQ ```

`a` chunked to size `b`.


## CMD: [``` % ```](#cmd--30)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a (mod b)`


## CMD: [``` /% ```](#cmd--31)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' NUM' ```

Results of [``` /~ ```](#cmd--27) and [``` % ```](#cmd--30) on `a` and `b`.


## CMD: [``` %% ```](#cmd--32)

Stack: ``` (a >STR)' (b >NUM)' -> ARR[STR]' ```

Atomic [``` %` ```](#cmd--33).


## CMD: [``` %` ```](#cmd--33)

Stack: ``` a (b >NUM)' -> SEQ ```

`a` windowed to size `b`.


## CMD: [``` ^ ```](#cmd--34)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a ^ b`. Throws error if result would be a complex number.


## CMD: [``` ^~ ```](#cmd--35)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

[``` ^ ```](#cmd--34) but `b` is coerced to `int`.


## CMD: [``` e^ ```](#cmd-e-3)

Stack: ``` @s (a >NUM)' -> NUM' ```

`e ^ a`


## CMD: [``` abs ```](#cmd-abs)

Stack: ``` @s (a >NUM)' -> NUM' ```

Absolute value of `a`.


## CMD: [``` sin ```](#cmd-sin)

Stack: ``` @s (a >NUM)' -> NUM' ```

Sine of `a`.


## CMD: [``` cos ```](#cmd-cos)

Stack: ``` @s (a >NUM)' -> NUM' ```

Cosine of `a`.


## CMD: [``` tan ```](#cmd-tan)

Stack: ``` @s (a >NUM)' -> NUM' ```

Tangent of `a`.


## CMD: [``` sin_ ```](#cmd-sin_)

Stack: ``` @s (a >NUM)' -> NUM' ```

Arcsine of `a`.


## CMD: [``` cos_ ```](#cmd-cos_)

Stack: ``` @s (a >NUM)' -> NUM' ```

Arccosine of `a`.


## CMD: [``` tan_ ```](#cmd-tan_)

Stack: ``` @s (a >NUM)' -> NUM' ```

Arctangent of `a`.


## CMD: [``` tan_II ```](#cmd-tan_ii)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

Arctangent of `a` with `b` as quadrant.


## CMD: [``` sinh ```](#cmd-sinh)

Stack: ``` @s (a >NUM)' -> NUM' ```

Hyperbolic sine of `a`.


## CMD: [``` cosh ```](#cmd-cosh)

Stack: ``` @s (a >NUM)' -> NUM' ```

Hyperbolic cosine of `a`.


## CMD: [``` tanh ```](#cmd-tanh)

Stack: ``` @s (a >NUM)' -> NUM' ```

Hyperbolic tangent of `a`.


## CMD: [``` sinh_ ```](#cmd-sinh_)

Stack: ``` @s (a >NUM)' -> NUM' ```

Hyperbolic arcsine of `a`.


## CMD: [``` cosh_ ```](#cmd-cosh_)

Stack: ``` @s (a >NUM)' -> NUM' ```

Hyperbolic arccosine of `a`.


## CMD: [``` tanh_ ```](#cmd-tanh_)

Stack: ``` @s (a >NUM)' -> NUM' ```

Hyperbolic arctangent of `a`.


## CMD: [``` log ```](#cmd-log)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

Base `b` logarithm of `a`.


## CMD: [``` ln ```](#cmd-ln)

Stack: ``` @s (a >NUM)' -> NUM' ```

Natural logarithm of `a`.


## CMD: [``` logX ```](#cmd-logx)

Stack: ``` @s (a >NUM)' -> NUM' ```

Base-10 logarithm of `a`.


## CMD: [``` P? ```](#cmd-p-1)

Stack: ``` @s (a >NUM)' -> NUM' ```

Whether `a` is prime. Uses a strong pseudo-primality test with a 1/1e12 chance of being wrong.


## CMD: [``` P/ ```](#cmd-p-2)

Stack: ``` (a >NUM)' -> MAP[(NUM => NUM)*] ```

Prime-factorizes `a` into pairs of prime `y` and frequency `z`.


## CMD: [``` ! ```](#cmd--36)

Stack: ``` a' -> NUM' ```

Atomic [``` !` ```](#cmd--37).


## CMD: [``` !` ```](#cmd--37)

Stack: ``` a -> NUM ```

Logical NOT.


## CMD: [``` & ```](#cmd--38)

Stack: ``` a' b' -> (a | b)' ```

Atomic [``` &` ```](#cmd--40).


## CMD: [``` && ```](#cmd--39)

Stack: ``` a' b' -> NUM' ```

Atomic [``` &&` ```](#cmd--41).


## CMD: [``` &` ```](#cmd--40)

Stack: ``` a b -> a | b ```

Minimum of `a` and `b`.


## CMD: [``` &&` ```](#cmd--41)

Stack: ``` a b -> NUM ```

Logical AND of `a` and `b`.


## CMD: [``` | ```](#cmd--42)

Stack: ``` a' b' -> (a | b)' ```

Atomic [``` |` ```](#cmd--44).


## CMD: [``` || ```](#cmd--43)

Stack: ``` a' b' -> NUM' ```

Atomic [``` ||` ```](#cmd--45).


## CMD: [``` |` ```](#cmd--44)

Stack: ``` a b -> a | b ```

Maximum of `a` and `b`.


## CMD: [``` ||` ```](#cmd--45)

Stack: ``` a b -> NUM ```

Logical OR of `a` and `b`.


## CMD: [``` <=> ```](#cmd--46)

Stack: ``` a' b' -> NUM' ```

Atomic [``` <=>` ```](#cmd--47).


## CMD: [``` <=>` ```](#cmd--47)

Stack: ``` a b -> NUM ```

Comparison (-1, 0, or 1 depending on whether `a` is less than, equal to, or greater than `b`).


## CMD: [``` = ```](#cmd--48)

Stack: ``` a' b' -> NUM' ```

Atomic [``` =` ```](#cmd--49).


## CMD: [``` =` ```](#cmd--49)

Stack: ``` a b -> NUM ```

Whether `a` equals `b`.


## CMD: [``` != ```](#cmd--50)

Stack: ``` a' b' -> NUM' ```

Atomic [``` !=` ```](#cmd--51).


## CMD: [``` !=` ```](#cmd--51)

Stack: ``` a b -> NUM ```

Whether `a` does not equals `b`.


## CMD: [``` < ```](#cmd--52)

Stack: ``` a' b' -> NUM' ```

Atomic [``` <` ```](#cmd--53).


## CMD: [``` <` ```](#cmd--53)

Stack: ``` a b -> NUM ```

Whether `a` is less than `b`.


## CMD: [``` > ```](#cmd--54)

Stack: ``` a' b' -> NUM' ```

Atomic [``` >` ```](#cmd--55).


## CMD: [``` >` ```](#cmd--55)

Stack: ``` a b -> NUM ```

Whether `a` is greater than `b`.


## CMD: [``` <= ```](#cmd--56)

Stack: ``` a' b' -> NUM' ```

Atomic [``` <=` ```](#cmd--57).


## CMD: [``` <=` ```](#cmd--57)

Stack: ``` a b -> NUM ```

Whether `a` is less than or equal to `b`.


## CMD: [``` >= ```](#cmd--58)

Stack: ``` a' b' -> NUM' ```

Atomic [``` >=` ```](#cmd--59).


## CMD: [``` >=` ```](#cmd--59)

Stack: ``` a b -> NUM ```

Whether `a` is greater than or equal to `b`.


## CMD: [``` : ```](#cmd--60)

Stack: ``` a i' -> (a._ | UN)' ```

Value at atomic index `i` in `a`.


## CMD: [``` :r ```](#cmd-r)

Stack: ``` a -> a._ ```

Value at random index in `a`.


## CMD: [``` :` ```](#cmd--61)

Stack: ``` a i -> a._ | UN ```

Value at index `i` in `a`.


## CMD: [``` :? ```](#cmd--62)

Stack: ``` a b' -> NUM' ```

Whether `a` has atomic `b`.


## CMD: [``` :?` ```](#cmd--63)

Stack: ``` a b -> NUM ```

Whether `a` has `b`.`MAP`s check `b` against keys; other types of `a` check `b` against values.


## CMD: [``` len ```](#cmd-len)

Stack: ``` a -> NUM ```

Length of `a`.


## CMD: [``` , ```](#cmd--64)

Stack: ``` a b -> ARR[a b] ```

Pairs `a` and `b` in an `ARR`.


## CMD: [``` ,, ```](#cmd--65)

Stack: ``` a -> ARR[a] ```

Wraps `a` in an `ARR`.


## CMD: [``` ,` ```](#cmd--66)

Stack: ``` a* -> a ```

Wraps stack in an `ARR`.


## CMD: [``` ,_ ```](#cmd-_-4)

Stack: ``` a -> a* ```

Unwraps `a`.


## CMD: [``` ,,_ ```](#cmd-_-5)

Stack: ``` _* a -> a* ```

Replaces stack with `a` unwrapped.


## CMD: [``` tk ```](#cmd-tk)

Stack: ``` a (n >NUM)' -> _ ```

Takes up to `n` items from `a`.Negative `n` takes from the end instead of the start.


## CMD: [``` dp ```](#cmd-dp)

Stack: ``` a (n >NUM)' -> _ ```

Drops up to `n` items from `a`.Negative `n` drops from the end instead of the start.


## CMD: [``` flat ```](#cmd-flat)

Stack: ``` a -> _ ```

Flattens `a`.


## CMD: [``` rep ```](#cmd-rep)

Stack: ``` a -> SEQ ```

Infinite `SEQ` with `a` repeated.


## CMD: [``` cyc ```](#cmd-cyc)

Stack: ``` a -> SEQ ```

Infinite `SEQ` with items of `a` cycled.


## CMD: [``` itr ```](#cmd-itr)

Stack: ``` a (f: b -> _) -> SEQ ```

Infinite `SEQ` of `f` successively [``` Q ```](#cmd-q-1)ed to `a`.i.e. `a f(a) f(f(a)) ...`


## CMD: [``` fold_ ```](#cmd-fold_)

Stack: ``` a (f: b -> _ _ | ) -> SEQ ```

`SEQ` generated from `f` successively [``` Q ```](#cmd-q-1)ed to `a`,where `x` is the new current item and `y` is the next `b` to be subsequently [``` Q ```](#cmd-q-1)ed to `f`.Generation stops if `f` [``` Q ```](#cmd-q-1)ed to `a` results in an empty stack.


## CMD: [``` >kv ```](#cmd-kv)

Stack: ``` a -> SEQ[ARR[k v]*] ```

`SEQ` of key/value pairs in `a`.


## CMD: [``` >k ```](#cmd-k)

Stack: ``` a -> SEQ ```

`SEQ` of keys in `a`.


## CMD: [``` >v ```](#cmd-v)

Stack: ``` a -> SEQ ```

`SEQ` of values in `a`.


## CMD: [``` a>b ```](#cmd-ab)

Stack: ``` (a >NUM)' (b >NUM)' -> SEQ[NUM*]' ```

Exclusive range from `a` to `b`.


## CMD: [``` O>a ```](#cmd-oa)

Stack: ``` (a >NUM)' -> SEQ[NUM*]' ```

Exclusive range from 0 to `a`.


## CMD: [``` a>O ```](#cmd-ao)

Stack: ``` (a >NUM)' -> SEQ[NUM*]' ```

Exclusive range from `a` to 0.


## CMD: [``` I>a ```](#cmd-ia)

Stack: ``` (a >NUM)' -> SEQ[NUM*]' ```

Exclusive range from 1 to `a`.


## CMD: [``` a>I ```](#cmd-ai)

Stack: ``` (a >NUM)' -> SEQ[NUM*]' ```

Exclusive range from `a` to 1.


## CMD: [``` shuf ```](#cmd-shuf)

Stack: ``` a -> _ ```

Shuffles `a`.


## CMD: [``` perm ```](#cmd-perm)

Stack: ``` a -> SEQ ```

All permutations of `a`.


## CMD: [``` comb ```](#cmd-comb)

Stack: ``` a (n >NUM)' -> SEQ' ```

All length-`n` combinations of `a`.


## CMD: [``` ^set ```](#cmd-set)

Stack: ``` a -> SEQ ```

All subsets of `a`.

