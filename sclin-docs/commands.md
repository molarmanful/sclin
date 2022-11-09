
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

Stack: ``` a -> 0 | 1 ```

1 or 0 depending on truthiness of `a`.


## CMD: [``` N>d ```](#cmd-nd)

Stack: ``` (a >NUM) (b >NUM)' -> STR ```

Converts `a` to an `STR` formatted to `b`'s specifications.


## CMD: [``` >TT ```](#cmd-tt)

Stack: ``` a b -> _ ```

Converts `a` to type of `b`.


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


## CMD: [``` $L* ```](#cmd-l-1)

Stack: ``` -> ARR[STR*] ```

`ARR` of lines of currently-executing file.


## CMD: [``` $ABC ```](#cmd-abc)

Stack: ``` -> STR ```

`UPPERCASE` alphabet.


## CMD: [``` $abc ```](#cmd-abc-1)

Stack: ``` -> STR ```

`lowercase` alphabet.


## CMD: [``` g@ ```](#cmd-g)

Stack: ``` -> STR | UN ```

Current line.


## CMD: [``` g; ```](#cmd-g-1)

Stack: ``` -> STR | UN ```

Next line.


## CMD: [``` g;; ```](#cmd-g-2)

Stack: ``` -> STR | UN ```

Previous line.


## CMD: [``` n\ ```](#cmd-n-2)

Stack: ``` -> STR ```

Newline character.


## CMD: [``` @$ ```](#cmd--4)

Stack: ``` (a >STR) -> ```

Loads ID `a` into local scope.
```sclin
"outer"=$a ( \a @$ a ) # $a
#a "inner"
```


## CMD: [``` @$$ ```](#cmd--5)

Stack: ``` (a >STR) -> ```

Loads ID `a` into global scope.
```sclin
\a @$$ ( "inner" =$a $a ) # a
#a "outer"
```


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

[``` pop ```](#cmd-pop)s `b`, [``` # ```](#cmd--7)s `f`, and pushes `b`.


## CMD: [``` \ ```](#cmd--6)

Stack: ``` a -> FN[a] ```

Wraps `a` in `FN`.


## CMD: [``` # ```](#cmd--7)

Stack: ``` a* f -> _* ```

Executes `f`.
```sclin
1 2 ( 3 + 4 ) #
```


## CMD: [``` Q ```](#cmd-q-1)

Stack: ``` f' -> _' ```

Evaluates `f` ([``` # ```](#cmd--7) but only preserves resulting top of stack).
```sclin
1 2 ( dups 3+` ) Q
```


## CMD: [``` @@ ```](#cmd--8)

Stack: ``` a* (n >NUM) -> _* ```

[``` # ```](#cmd--7)s `n`th line.


## CMD: [``` @~ ```](#cmd--9)

Stack: ``` a* (n >NUM) -> _* ```

[``` # ```](#cmd--7)s `n`th line relative to current line.


## CMD: [``` @ ```](#cmd--10)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--7)s current line.


## CMD: [``` ; ```](#cmd--11)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--7)s next line.


## CMD: [``` ;; ```](#cmd--12)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--7)s previous line.


## CMD: [``` g@@ ```](#cmd-g-3)

Stack: ``` (n >NUM) -> STR | UN ```

`n`th line.


## CMD: [``` g@~ ```](#cmd-g-4)

Stack: ``` (n >NUM) -> STR | UN ```

`n`th line relative to current line.


## CMD: [``` &# ```](#cmd--13)

Stack: ``` a* (b >(0 | 1)) f -> _* ```

[``` # ```](#cmd--7)s `f` if `b` is truthy.


## CMD: [``` |# ```](#cmd--14)

Stack: ``` a* (b >(0 | 1)) f -> _* ```

[``` # ```](#cmd--7)s `f` if `b` is falsy.


## CMD: [``` ?# ```](#cmd--15)

Stack: ``` a* (b >(0 | 1)) f g -> _* ```

[``` # ```](#cmd--7)s `f` if `b` is truthy; else [``` # ```](#cmd--7)s `g`.


## CMD: [``` *# ```](#cmd--16)

Stack: ``` a* f (n >NUM) -> _* ```

[``` # ```](#cmd--7)s `f` `n` times.


## CMD: [``` !# ```](#cmd--17)

Stack: ``` a* f g -> _* ```

Tries to [``` # ```](#cmd--7) `f`; on error, pushes caught `ERR` and [``` # ```](#cmd--7)s `g`.


## CMD: [``` >! ```](#cmd--18)

Stack: ``` (e ERR) -> ```

Throws `e`.


## CMD: [``` ' ```](#cmd--19)

Stack: ``` (a >ARR) f -> ARR ```

[``` # ```](#cmd--7)s `f` on `a` as if it were a stack.
```sclin
[1 2 3 4] ( 5 swap ) '
```


## CMD: [``` '_ ```](#cmd-_)

Stack: ``` (a* >ARR) f -> _* ```

[``` # ```](#cmd--7)s `f` on the stack as if it were an `ARR`.
```sclin
1 2 3 4 1.+.map '_
```


## CMD: [``` end ```](#cmd-end)

Stack: ``` -> ```

Clears code queue, similar to the "break" keyword in other languages.


## CMD: [``` E ```](#cmd-e-2)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a * 10 ^ b`


## CMD: [``` I ```](#cmd-i-1)

Stack: ``` (a >NUM)' -> NUM' ```

Rounds `a` towards 0.


## CMD: [``` I? ```](#cmd-i-2)

Stack: ``` (a >NUM)' -> (1 | 0)' ```

Whether `a` is an integer.


## CMD: [``` |_ ```](#cmd-_-1)

Stack: ``` (a >NUM)' -> NUM' ```

Rounds `a` towards -∞.


## CMD: [``` |~ ```](#cmd--20)

Stack: ``` (a >NUM)' -> NUM' ```

Rounds `a` to nearest integer.


## CMD: [``` |^ ```](#cmd--21)

Stack: ``` (a >NUM)' -> NUM' ```

Rounds `a` towards ∞.


## CMD: [``` X>b ```](#cmd-xb)

Stack: ``` (a >NUM)' (b >NUM)' -> ARR[NUM]' ```

Converts `a` from decimal to `ARR` of base-`b` digits.
```sclin
153 2X>b
```
```sclin
153 16X>b
```


## CMD: [``` b>X ```](#cmd-bx)

Stack: ``` (a >ARR[>NUM]) (b >NUM)' -> NUM' ```

Converts base-`b` digits to decimal.
```sclin
[1 0 0 1 1 0 0 1] 2b>X
```
```sclin
[9 9] 16b>X
```


## CMD: [``` >n/d ```](#cmd-nd-1)

Stack: ``` (a >NUM)' -> ARR[NUM NUM]' ```

Converts `a` to a numerator-denominator pair.
```sclin
4 6/ >n/d
```
```sclin
$PI >n/d
```


## CMD: [``` prec? ```](#cmd-prec)

Stack: ``` (a >NUM)' -> ARR[NUM NUM]' ```

Whether `a` is an exact value (i.e. represented in full precision).
```sclin
2 3/ prec?
```
```sclin
$PI prec?
```


## CMD: [``` _ ```](#cmd-_-2)

Stack: ``` (a >NUM)' -> NUM' ```

`-a`


## CMD: [``` __ ```](#cmd-__)

Stack: ``` (a >STR)' -> STR' ```

Atomic [``` _` ```](#cmd-_-3).


## CMD: [``` _` ```](#cmd-_-3)

Stack: ``` a -> _ ```

Reverses `a`.


## CMD: [``` + ```](#cmd--22)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a + b`


## CMD: [``` ++ ```](#cmd--23)

Stack: ``` (a >STR)' (b >STR)' -> STR' ```

Atomic [``` +` ```](#cmd--24).


## CMD: [``` +` ```](#cmd--24)

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

Remove occurrences of `b` from `a`.
If `a` is `MAP`, then removal is performed on keys instead of values.
```sclin
[1 2 3 4] 2-`
```
```sclin
{0 1, 2 3, } 2-`
```


## CMD: [``` * ```](#cmd--25)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a * b`


## CMD: [``` ** ```](#cmd--26)

Stack: ``` (a >STR)' (b >NUM)' -> STR' ```

Atomic [``` *` ```](#cmd--27).


## CMD: [``` *` ```](#cmd--27)

Stack: ``` a b -> _ ```

`a` replicated according to `b`.
If `b` is iterable, then `a` and `b` are recursively zipped together and replicated.
```sclin
[1 2 3 4] [0 2 0 3] *` >A
```
```sclin
[1 2 3 4] 3*` >A
```


## CMD: [``` / ```](#cmd--28)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a / b`. Throws error if `b` is 0.


## CMD: [``` /~ ```](#cmd--29)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

Integer [``` / ```](#cmd--28).


## CMD: [``` // ```](#cmd--30)

Stack: ``` (a >STR)' (b >NUM)' -> SEQ[STR]' ```

Atomic [``` /` ```](#cmd--31).


## CMD: [``` /` ```](#cmd--31)

Stack: ``` a (b >NUM)' -> SEQ ```

`a` chunked to size `b`.
```sclin
[1 2 3 4 5] 2/` >A
```


## CMD: [``` % ```](#cmd--32)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a (mod b)`


## CMD: [``` /% ```](#cmd--33)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' NUM' ```

Results of [``` /~ ```](#cmd--29) and [``` % ```](#cmd--32) on `a` and `b`.


## CMD: [``` %% ```](#cmd--34)

Stack: ``` (a >STR)' (b >NUM)' -> SEQ[STR]' ```

Atomic [``` %` ```](#cmd--35).


## CMD: [``` %` ```](#cmd--35)

Stack: ``` a (b >NUM)' -> SEQ ```

`a` windowed to size `b`.
```sclin
[1 2 3 4 5] 3%` >A
```


## CMD: [``` ^ ```](#cmd--36)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a ^ b`. Throws error if result would be a complex number.


## CMD: [``` ^~ ```](#cmd--37)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

[``` ^ ```](#cmd--36) but `b` is coerced to `int`.


## CMD: [``` ^^ ```](#cmd--38)

Stack: ``` (a >STR)' (b >NUM)' -> SEQ[STR]' ```

Atomic [``` ^` ```](#cmd--39).


## CMD: [``` ^` ```](#cmd--39)

Stack: ``` a (n >NUM)' -> SEQ' ```

Cartesian power of seed `a` to `n`.
```sclin
"abc" 3^` >A
```


## CMD: [``` e^ ```](#cmd-e-3)

Stack: ``` (a >NUM)' -> NUM' ```

`e ^ a`


## CMD: [``` abs ```](#cmd-abs)

Stack: ``` (a >NUM)' -> NUM' ```

Absolute value of `a`.


## CMD: [``` sin ```](#cmd-sin)

Stack: ``` (a >NUM)' -> NUM' ```

Sine of `a`.


## CMD: [``` cos ```](#cmd-cos)

Stack: ``` (a >NUM)' -> NUM' ```

Cosine of `a`.


## CMD: [``` tan ```](#cmd-tan)

Stack: ``` (a >NUM)' -> NUM' ```

Tangent of `a`.


## CMD: [``` sin_ ```](#cmd-sin_)

Stack: ``` (a >NUM)' -> NUM' ```

Arcsine of `a`.


## CMD: [``` cos_ ```](#cmd-cos_)

Stack: ``` (a >NUM)' -> NUM' ```

Arccosine of `a`.


## CMD: [``` tan_ ```](#cmd-tan_)

Stack: ``` (a >NUM)' -> NUM' ```

Arctangent of `a`.


## CMD: [``` tan_II ```](#cmd-tan_ii)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

Arctangent of `a` with `b` as quadrant.


## CMD: [``` sinh ```](#cmd-sinh)

Stack: ``` (a >NUM)' -> NUM' ```

Hyperbolic sine of `a`.


## CMD: [``` cosh ```](#cmd-cosh)

Stack: ``` (a >NUM)' -> NUM' ```

Hyperbolic cosine of `a`.


## CMD: [``` tanh ```](#cmd-tanh)

Stack: ``` (a >NUM)' -> NUM' ```

Hyperbolic tangent of `a`.


## CMD: [``` sinh_ ```](#cmd-sinh_)

Stack: ``` (a >NUM)' -> NUM' ```

Hyperbolic arcsine of `a`.


## CMD: [``` cosh_ ```](#cmd-cosh_)

Stack: ``` (a >NUM)' -> NUM' ```

Hyperbolic arccosine of `a`.


## CMD: [``` tanh_ ```](#cmd-tanh_)

Stack: ``` (a >NUM)' -> NUM' ```

Hyperbolic arctangent of `a`.


## CMD: [``` log ```](#cmd-log)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

Base `b` logarithm of `a`.


## CMD: [``` ln ```](#cmd-ln)

Stack: ``` (a >NUM)' -> NUM' ```

Natural logarithm of `a`.


## CMD: [``` logX ```](#cmd-logx)

Stack: ``` (a >NUM)' -> NUM' ```

Base-10 logarithm of `a`.


## CMD: [``` P? ```](#cmd-p-1)

Stack: ``` (a >NUM)' -> NUM' ```

Whether `a` is prime. Uses a strong pseudo-primality test with a 1/1e12 chance of being wrong.


## CMD: [``` P/ ```](#cmd-p-2)

Stack: ``` (a >NUM)' -> MAP[(NUM => NUM)*] ```

Prime-factorizes `a` into pairs of prime `y` and frequency `z`.
```sclin
340P/
```


## CMD: [``` ! ```](#cmd--40)

Stack: ``` a' -> (0 | 1)' ```

Atomic [``` !` ```](#cmd--41).


## CMD: [``` !` ```](#cmd--41)

Stack: ``` a -> 0 | 1 ```

Logical NOT.


## CMD: [``` & ```](#cmd--42)

Stack: ``` a' b' -> (a | b)' ```

Atomic [``` &` ```](#cmd--44).


## CMD: [``` && ```](#cmd--43)

Stack: ``` a' b' -> (0 | 1)' ```

Atomic [``` &&` ```](#cmd--45).


## CMD: [``` &` ```](#cmd--44)

Stack: ``` a b -> a | b ```

Minimum of `a` and `b`.


## CMD: [``` &&` ```](#cmd--45)

Stack: ``` a b -> 0 | 1 ```

Logical AND of `a` and `b`.


## CMD: [``` | ```](#cmd--46)

Stack: ``` a' b' -> (a | b)' ```

Atomic [``` |` ```](#cmd--48).


## CMD: [``` || ```](#cmd--47)

Stack: ``` a' b' -> (0 | 1)' ```

Atomic [``` ||` ```](#cmd--49).


## CMD: [``` |` ```](#cmd--48)

Stack: ``` a b -> a | b ```

Maximum of `a` and `b`.


## CMD: [``` ||` ```](#cmd--49)

Stack: ``` a b -> 0 | 1 ```

Logical OR of `a` and `b`.


## CMD: [``` <=> ```](#cmd--50)

Stack: ``` a' b' -> (-1 | 0 | 1)' ```

Atomic [``` <=>` ```](#cmd--51).


## CMD: [``` <=>` ```](#cmd--51)

Stack: ``` a b -> -1 | 0 | 1 ```

Comparison (-1, 0, or 1 depending on whether `a` is less than, equal to, or greater than `b`).


## CMD: [``` = ```](#cmd--52)

Stack: ``` a' b' -> (0 | 1)' ```

Atomic [``` =` ```](#cmd--53).


## CMD: [``` =` ```](#cmd--53)

Stack: ``` a b -> 0 | 1 ```

Whether `a` equals `b`.


## CMD: [``` != ```](#cmd--54)

Stack: ``` a' b' -> (0 | 1)' ```

Atomic [``` !=` ```](#cmd--55).


## CMD: [``` !=` ```](#cmd--55)

Stack: ``` a b -> 0 | 1 ```

Whether `a` does not equals `b`.


## CMD: [``` < ```](#cmd--56)

Stack: ``` a' b' -> (0 | 1)' ```

Atomic [``` <` ```](#cmd--57).


## CMD: [``` <` ```](#cmd--57)

Stack: ``` a b -> 0 | 1 ```

Whether `a` is less than `b`.


## CMD: [``` > ```](#cmd--58)

Stack: ``` a' b' -> (0 | 1)' ```

Atomic [``` >` ```](#cmd--59).


## CMD: [``` >` ```](#cmd--59)

Stack: ``` a b -> 0 | 1 ```

Whether `a` is greater than `b`.


## CMD: [``` <= ```](#cmd--60)

Stack: ``` a' b' -> (0 | 1)' ```

Atomic [``` <=` ```](#cmd--61).


## CMD: [``` <=` ```](#cmd--61)

Stack: ``` a b -> 0 | 1 ```

Whether `a` is less than or equal to `b`.


## CMD: [``` >= ```](#cmd--62)

Stack: ``` a' b' -> (0 | 1)' ```

Atomic [``` >=` ```](#cmd--63).


## CMD: [``` >=` ```](#cmd--63)

Stack: ``` a b -> 0 | 1 ```

Whether `a` is greater than or equal to `b`.


## CMD: [``` : ```](#cmd--64)

Stack: ``` a i' -> (a._ | UN)' ```

Value at atomic index `i` in `a`.


## CMD: [``` :r ```](#cmd-r)

Stack: ``` a -> a._ ```

Value at random index in `a`.


## CMD: [``` :` ```](#cmd--65)

Stack: ``` a i -> a._ | UN ```

Value at index `i` in `a`.


## CMD: [``` := ```](#cmd--66)

Stack: ``` a >ARR[i b] -> x ```

Sets value at index `i` in `a` to `b`.


## CMD: [``` :- ```](#cmd---2)

Stack: ``` a i -> x ```

Removes index `i` from `a`.


## CMD: [``` :? ```](#cmd--67)

Stack: ``` a b' -> (0 | 1)' ```

Whether `a` has atomic `b`.


## CMD: [``` :?` ```](#cmd--68)

Stack: ``` a b -> 0 | 1 ```

Whether `a` has `b`.
`MAP`s check `b` against keys; other types of `a` check `b` against values.


## CMD: [``` len ```](#cmd-len)

Stack: ``` a -> NUM ```

Length of `a`.


## CMD: [``` , ```](#cmd--69)

Stack: ``` a b -> ARR[a b] ```

Pairs `a` and `b` in an `ARR`.


## CMD: [``` ,, ```](#cmd--70)

Stack: ``` a -> ARR[a] ```

Wraps `a` in an `ARR`.


## CMD: [``` ,` ```](#cmd--71)

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

Takes up to `n` items from `a`.
Negative `n` takes from the end instead of the start.


## CMD: [``` dp ```](#cmd-dp)

Stack: ``` a (n >NUM)' -> _ ```

Drops up to `n` items from `a`.
Negative `n` drops from the end instead of the start.


## CMD: [``` flat ```](#cmd-flat)

Stack: ``` a -> _ ```

Flattens `a` by one depth.


## CMD: [``` rep ```](#cmd-rep)

Stack: ``` a -> SEQ ```

Infinite `SEQ` with `a` repeated.
```sclin
5rep 10tk >A
```
```sclin
[1 2 3] rep 10tk >A
```


## CMD: [``` cyc ```](#cmd-cyc)

Stack: ``` a -> SEQ ```

Infinite `SEQ` with elements of `a` cycled.
```sclin
[1 2 3] cyc 10tk >A
```


## CMD: [``` itr ```](#cmd-itr)

Stack: ``` a (f: b -> _) -> SEQ ```

Infinite `SEQ` of `f` successively [``` Q ```](#cmd-q-1)ed to `a`.
```sclin
1 1.+ itr 10tk >A
```
```sclin
1 ( 1+ 1 swap / ) itr 10tk >A
```


## CMD: [``` fold_ ```](#cmd-fold_)

Stack: ``` a (f: b -> _ _ | ) -> SEQ ```

`SEQ` generated from `f` successively [``` Q ```](#cmd-q-1)ed to `a`,
where `x` is the new current item and `y` is the next `b` to be subsequently [``` Q ```](#cmd-q-1)ed to `f`.
Generation stops if `f` [``` Q ```](#cmd-q-1)ed to `a` results in an empty stack.
```sclin
0 1, ( ,_ tuck + dups \swap dip ) fold_ 10tk >A
```


## CMD: [``` >kv ```](#cmd-kv)

Stack: ``` a -> (SEQ | ARR)[ARR[k v]*] ```

`SEQ` of key/value pairs in `a`.
```sclin
["a" "b" "c" "d"] >kv >A
```
```sclin
{"x""a", "y""b", "z""c", } >kv >A
```


## CMD: [``` =>kv ```](#cmd-kv-1)

Stack: ``` a -> MAP[(_ => _)*] ```

[``` >kv ```](#cmd-kv) and [``` >M ```](#cmd-m).
```sclin
["a" "b" "c" "d"] =>kv
```


## CMD: [``` >k ```](#cmd-k)

Stack: ``` a -> SEQ | ARR ```

Keys in `a`.
```sclin
{"x" "a", "y" "b", "z" "c", } >k >A
```


## CMD: [``` >v ```](#cmd-v)

Stack: ``` a -> SEQ | ARR ```

Values in `a`.
```sclin
{"x""a", "y""b", "z""c", } >v >A
```


## CMD: [``` a>b ```](#cmd-ab)

Stack: ``` (a >NUM)' (b >NUM)' -> ARR[NUM*]' ```

Exclusive range from `a` to `b`.


## CMD: [``` O>a ```](#cmd-oa)

Stack: ``` (a >NUM)' -> ARR[NUM*]' ```

Exclusive range from 0 to `a`.


## CMD: [``` a>O ```](#cmd-ao)

Stack: ``` (a >NUM)' -> ARR[NUM*]' ```

Exclusive range from `a` to 0.


## CMD: [``` I>a ```](#cmd-ia)

Stack: ``` (a >NUM)' -> ARR[NUM*]' ```

Exclusive range from 1 to `a`.


## CMD: [``` a>I ```](#cmd-ai)

Stack: ``` (a >NUM)' -> ARR[NUM*]' ```

Exclusive range from `a` to 1.


## CMD: [``` shuf ```](#cmd-shuf)

Stack: ``` a -> _ ```

Shuffles `a`.
```sclin
10O>a shuf
```


## CMD: [``` perm ```](#cmd-perm)

Stack: ``` a -> SEQ ```

All permutations of `a`.
```sclin
[1 2 3] perm >A
```


## CMD: [``` comb ```](#cmd-comb)

Stack: ``` a (n >NUM)' -> SEQ' ```

All length-`n` combinations of `a`.
```sclin
[1 2 3] 2comb >A
```


## CMD: [``` ^set ```](#cmd-set)

Stack: ``` a -> SEQ ```

All subsets of `a`.
```sclin
[1 2 3] ^set >A
```


## CMD: [``` Q* ```](#cmd-q-2)

Stack: ``` a[_*] -> SEQ' ```

Cartesian product of iterable-of-iterables `a` to `n`.
```sclin
["abc" "def" "ghi"] Q* >A
```


## CMD: [``` S>c ```](#cmd-sc)

Stack: ``` (a >STR)' -> ARR[NUM]' ```

Converts `a` to codepoints.
```sclin
"hello"S>c
```


## CMD: [``` c>S ```](#cmd-cs)

Stack: ``` (a >ARR[NUM]) -> STR ```

Converts iterable of codepoints to `STR`.
```sclin
[104 101 108 108 111] c>S
```


## CMD: [``` <> ```](#cmd--72)

Stack: ``` (a >STR)' (b >STR)' -> ARR' ```

Splits `a` with `b`.


## CMD: [``` <>: ```](#cmd--73)

Stack: ``` a (i >NUM) -> ARR[_ _] ```

[``` tk ```](#cmd-tk) and [``` dp ```](#cmd-dp) of `a` at index `i`.


## CMD: [``` c<> ```](#cmd-c)

Stack: ``` (a >STR)' -> ARR' ```

[``` <> ```](#cmd--72)s with empty string.


## CMD: [``` w<> ```](#cmd-w-1)

Stack: ``` (a >STR)' -> ARR' ```

[``` <> ```](#cmd--72)s with space.


## CMD: [``` n<> ```](#cmd-n-3)

Stack: ``` (a >STR)' -> ARR' ```

[``` <> ```](#cmd--72)s with newline.


## CMD: [``` s<> ```](#cmd-s-1)

Stack: ``` (a >STR)' -> ARR' ```

[``` <> ```](#cmd--72)s on whitespace characters.


## CMD: [``` >< ```](#cmd--74)

Stack: ``` a (b >STR)' -> STR' ```

Joins `a` with `b`.


## CMD: [``` c>< ```](#cmd-c-1)

Stack: ``` a -> STR' ```

[``` >< ```](#cmd--74)s with empty string.


## CMD: [``` w>< ```](#cmd-w-2)

Stack: ``` a -> STR' ```

[``` >< ```](#cmd--74)s with space.


## CMD: [``` n>< ```](#cmd-n-4)

Stack: ``` a -> STR' ```

[``` >< ```](#cmd--74)s with newline.


## CMD: [``` A>a ```](#cmd-aa)

Stack: ``` (a >STR)' -> STR' ```

Converts `STR` to `lowercase`.


## CMD: [``` a>A ```](#cmd-aa-1)

Stack: ``` (a >STR)' -> STR' ```

Converts `STR` to `UPPERCASE`.


## CMD: [``` map ```](#cmd-map)

Stack: ``` a f' -> _' ```

[``` Q ```](#cmd-q-1)s `f` on each element of `a`.
If `a` is `MAP`, then the signature of `f` is `k v -> _ |`,
where `k=>v` is the key-value pair.
Otherwise, the signature of `f` is `x -> _ |`,
where `x` is the element.
```sclin
[1 2 3 4] 1.+ map
```
```sclin
{0 1, 2 3, 4 5, } ( over + ) map
```


## CMD: [``` tap ```](#cmd-tap)

Stack: ``` a f' -> a ```

[``` map ```](#cmd-map) but `a` is preserved (i.e. leaving only side effects of `f`).
```sclin
[1 2 3 4] ( 1+ n>o ) tap
```


## CMD: [``` zip ```](#cmd-zip)

Stack: ``` a b (f: x y -> _ |)' -> _' ```

[``` Q ```](#cmd-q-1)s `f` over each element-wise pair of `a` and `b`.
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


## CMD: [``` zip~ ```](#cmd-zip-1)

Stack: ``` a b c d (f: x y -> _ |)' -> _' ```

[``` zip ```](#cmd-zip) but instead of truncating,
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


## CMD: [``` tbl ```](#cmd-tbl)

Stack: ``` a b (f: x y -> _ |)' -> _' ```

[``` Q ```](#cmd-q-1)s `f` over each table-wise pair of `a` and `b`.
```sclin
[1 2 3 4] [2 3 4 5] \++ tbl
```


## CMD: [``` mapf ```](#cmd-mapf)

Stack: ``` a f' -> _' ```

[``` map ```](#cmd-map) and [``` flat ```](#cmd-flat).
```sclin
1224P/ \*` mapf
```


## CMD: [``` rmap ```](#cmd-rmap)

Stack: ``` a f' -> _' ```

Atomic/recursive [``` map ```](#cmd-map).
```sclin
[[1 2] 3 4 [5 [6 7]]] ( dup n>o ) rmap
```


## CMD: [``` fold ```](#cmd-fold)

Stack: ``` a b f' -> _' ```

[``` Q ```](#cmd-q-1)s `f` to combine each accumulator and element starting from initial accumulator `b`.
If `a` is `MAP`, then the signature of `f` is `k x v -> _ |`,
where `k=>v` is the key-value pair and `x` is the accumulator.
Otherwise, the signature of `f` is `x y -> _ |`,
where `x` is the accumulator and `y` is the value.
```sclin
[1 2 3 4] 0 \+ fold
```
```sclin
"1011"_` =>kv 0 ( rot 2 swap ^ * + ) fold
```


## CMD: [``` rfold ```](#cmd-rfold)

Stack: ``` a b f' -> _' ```

Atomic/recursive [``` fold ```](#cmd-fold).
```sclin
[[1 2] 3 4 [5 [6 7]]] 0 \+ rfold
```
```sclin
[[1 2] 3 4 [5 [6 7]]] [] \+` rfold
```


## CMD: [``` +/ ```](#cmd--75)

Stack: ``` a -> NUM' ```

Sum of `a`. Equivalent to `0 \+ rfold`.


## CMD: [``` */ ```](#cmd--76)

Stack: ``` a -> NUM' ```

Product of `a`. Equivalent to `1 \* rfold`.


## CMD: [``` scan ```](#cmd-scan)

Stack: ``` a b f' -> _' ```

[``` fold ```](#cmd-fold) with intermediate values.
```sclin
[1 2 3 4] 0 \+ scan
```


## CMD: [``` walk ```](#cmd-walk)

Stack: ``` a f' -> _' ```

A multi-purpose function for creating, modifying, and traversing nested structures.
```sclin
[[1 2] 3 4 { "a" 5, "b" [6 7] , }] ( dups f>o ) walk
```
```sclin
[[1 2] 3 4 { "a" 5, "b" [6 7] , }] ( dup len ( dup +` ) &# ) walk
```


## CMD: [``` fltr ```](#cmd-fltr)

Stack: ``` a f' -> _' ```

Keeps elements of `a` that satisfy predicate `f`.
If `a` is `MAP`, then the signature of `f` is `k v -> 0 | 1 |`,
where `k=>v` is the key-value pair.
Otherwise, the signature of `f` is `x -> 0 | 1 |`,
where `x` is the element.
```sclin
[5 1 2 4 3] 2.> fltr
```


## CMD: [``` any ```](#cmd-any)

Stack: ``` a f' -> (0 | 1)' ```

Whether any elements of `a` satisfy predicate `f`.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```sclin
[5 1 2 4 3] 2.> any
```


## CMD: [``` all ```](#cmd-all)

Stack: ``` a f' -> (0 | 1)' ```

Whether all elements of `a` satisfy predicate `f`.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```sclin
[5 1 2 4 3] 2.> all
```


## CMD: [``` tk* ```](#cmd-tk-1)

Stack: ``` a f' -> _' ```

Takes elements of `a` until [``` Q ```](#cmd-q-1)ing `f` is falsy.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```sclin
[5 1 2 4 3] 4.!= tk*
```


## CMD: [``` dp* ```](#cmd-dp-1)

Stack: ``` a f' -> _' ```

Drops elements of `a` while predicate `f` is truthy.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```sclin
[5 1 2 4 3] 4.!= dp*
```


## CMD: [``` find ```](#cmd-find)

Stack: ``` a f' -> _' ```

Finds first element of `a` where predicate `f` is truthy.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```sclin
[5 1 2 4 3] ( 2% ! ) find
```


## CMD: [``` uniq ```](#cmd-uniq)

Stack: ``` a f' -> _' ```

Uniquifies elements of `a` with mapper `f`.
See [``` map ```](#cmd-map) for the signature of `f`.
```sclin
[5 1 2 4 3] 3.% uniq
```


## CMD: [``` sort ```](#cmd-sort)

Stack: ``` a f' -> _' ```

Sorts elements of `a` with mapper `f`.
See [``` map ```](#cmd-map) for the signature of `f`.
```sclin
["a" "" "abc" "ab"] \len sort
```
```sclin
[1 2 3 4 5] \$rng sort
```


## CMD: [``` sort~ ```](#cmd-sort-1)

Stack: ``` a f' -> _' ```

Sorts elements of `a` with comparator `f`.
If `a` is `MAP`, then the signature of `f` is `j k v w -> 0 | 1 |`,
where `j=>v` and `k=>w` are key-value pairs to compare.
Otherwise, the signature of `f` is `x y -> 0 | 1 |`,
where `x` and `y` are elements to compare.
```sclin
[1 5 2 3 4] \< sort~
```
```sclin
[1 5 2 3 4] \> sort~
```


## CMD: [``` part ```](#cmd-part)

Stack: ``` a f' -> ARR[_ _]' ```

Separates `a` into 2 parts based on predicate `f`.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```sclin
[5 1 2 4 3] 2.> part
```


## CMD: [``` group ```](#cmd-group)

Stack: ``` a f' -> MAP' ```

Separates `a` groups based on `f`.
Each result of `f` becomes a key in the resulting `MAP`.
See [``` map ```](#cmd-map) for the signature of `f`.
```sclin
"abc"^set >A \len group
```


## CMD: [``` span ```](#cmd-span)

Stack: ``` a f' -> ARR[_ _]' ```

Equivalent to a combination of [``` tk* ```](#cmd-tk-1) and [``` dp* ```](#cmd-dp-1).
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```sclin
[5 1 2 4 3] 2.% span
```


## CMD: [``` pack ```](#cmd-pack)

Stack: ``` a f' -> _' ```

Groups consecutive duplicate runs of `a` based on predicate `f`.
See [``` sort~ ```](#cmd-sort-1) for the signature of `f`.
```sclin
[1 1 2 3 3 4 6 4 4] \=` pack
```

