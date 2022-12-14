
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


## CMD: [``` >~ ```](#cmd-)

Stack: ``` a -> TASK ```

Converts `a` to `TASK`.


## CMD: [``` ~> ```](#cmd--1)

Stack: ``` a -> FUT' ```

Converts `a` to `FUT`.


## CMD: [``` >!? ```](#cmd--2)

Stack: ``` a -> TRY ```

Converts `a` to `TRY`.


## CMD: [``` >? ```](#cmd--3)

Stack: ``` a -> TF ```

Converts `a` to `TF`.


## CMD: [``` N>d ```](#cmd-nd)

Stack: ``` (a >NUM) (b >NUM)' -> STR ```

Converts `a` to an `STR` formatted to `b`'s specifications.


## CMD: [``` >TT ```](#cmd-tt)

Stack: ``` a b -> _ ```

Converts `a` to type of `b`.


## CMD: [``` UN ```](#cmd-un)

Stack: ``` -> UN ```

`UN`


## CMD: [``` $T ```](#cmd-t)

Stack: ``` -> TF ```

True.


## CMD: [``` $F ```](#cmd-f-1)

Stack: ``` -> TF ```

False.


## CMD: [``` () ```](#cmd--4)

Stack: ``` -> FN ```

Empty `FN`.


## CMD: [``` [] ```](#cmd--5)

Stack: ``` -> ARR ```

Empty `ARR`.


## CMD: [``` {} ```](#cmd--6)

Stack: ``` -> MAP ```

Empty `MAP`.


## CMD: [``` ()~ ```](#cmd--7)

Stack: ``` -> TASK ```

Empty `TASK`.


## CMD: [``` ()! ```](#cmd--8)

Stack: ``` -> TRY ```

Empty `TRY`.


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


## CMD: [``` $LINE ```](#cmd-line)

Stack: ``` -> NUM ```

Current line number of program execution.


## CMD: [``` $FILE ```](#cmd-file)

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


## CMD: [``` $L* ```](#cmd-l)

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


## CMD: [``` @$ ```](#cmd--9)

Stack: ``` (a >STR) -> ```

Loads ID `a` into local scope.
```
"outer"=$a ( \a @$ a ) # $a
#a "inner"
-> "inner" "outer"
```


## CMD: [``` @$$ ```](#cmd--10)

Stack: ``` (a >STR) -> ```

Loads ID `a` into global scope.
```
\a @$$ ( "inner" =$a $a ) # a
#a "outer"
-> "inner" "outer"
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




## CMD: [``` dupd ```](#cmd-dupd)

Stack: ``` a b -> a a b ```




## CMD: [``` over ```](#cmd-over)

Stack: ``` a b -> a b a ```




## CMD: [``` ddup ```](#cmd-ddup)

Stack: ``` a b -> a b a b ```




## CMD: [``` edup ```](#cmd-edup)

Stack: ``` a b c -> a b c a b c ```




## CMD: [``` pick ```](#cmd-pick)

Stack: ``` (a @ n) b* (n >NUM) -> a b* a ```

[``` dup ```](#cmd-dup)s `n`th item from top of stack.


## CMD: [``` pop ```](#cmd-pop)

Stack: ``` _ -> ```




## CMD: [``` clr ```](#cmd-clr)

Stack: ``` _* -> ```




## CMD: [``` nip ```](#cmd-nip)

Stack: ``` _ b -> b ```




## CMD: [``` ppop ```](#cmd-ppop)

Stack: ``` _ _ -> ```




## CMD: [``` qpop ```](#cmd-qpop)

Stack: ``` _ _ _ -> ```




## CMD: [``` nix ```](#cmd-nix)

Stack: ``` (a @ n) b* (n >NUM) -> _* ```

[``` pop ```](#cmd-pop)s `n`th item from top of stack.


## CMD: [``` swap ```](#cmd-swap)

Stack: ``` a b -> b a ```




## CMD: [``` rev ```](#cmd-rev)

Stack: ``` a* -> _* ```

Reverses stack.


## CMD: [``` swapd ```](#cmd-swapd)

Stack: ``` a b c -> b a c ```




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

[``` pop ```](#cmd-pop)s `b`, [``` # ```](#cmd--12)s `f`, and pushes `b`.


## CMD: [``` \ ```](#cmd--11)

Stack: ``` a -> FN[a] ```

Wraps `a` in `FN`.


## CMD: [``` # ```](#cmd--12)

Stack: ``` a* f -> _* ```

Executes `f`.
```
1 2 ( 3 + 4 ) #
-> 1 5 4
```


## CMD: [``` Q ```](#cmd-q-1)

Stack: ``` f' -> _' ```

Evaluates `f` ([``` # ```](#cmd--12) but only preserves resulting top of stack).
```
1 2 ( dups 3+` ) Q
-> 1 2 [1 2 3]
```


## CMD: [``` @@ ```](#cmd--13)

Stack: ``` a* (n >NUM) -> _* ```

[``` # ```](#cmd--12)s `n`th line.


## CMD: [``` @~ ```](#cmd--14)

Stack: ``` a* (n >NUM) -> _* ```

[``` # ```](#cmd--12)s `n`th line relative to current line.


## CMD: [``` @ ```](#cmd--15)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--12)s current line.


## CMD: [``` ; ```](#cmd--16)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--12)s next line.


## CMD: [``` ;; ```](#cmd--17)

Stack: ``` a* -> _* ```

[``` # ```](#cmd--12)s previous line.


## CMD: [``` g@@ ```](#cmd-g-3)

Stack: ``` (n >NUM) -> STR | UN ```

`n`th line.


## CMD: [``` g@~ ```](#cmd-g-4)

Stack: ``` (n >NUM) -> STR | UN ```

`n`th line relative to current line.


## CMD: [``` &# ```](#cmd--18)

Stack: ``` a* (b >TF) f -> _* ```

[``` # ```](#cmd--12)s `f` if `b` is truthy.


## CMD: [``` |# ```](#cmd--19)

Stack: ``` a* (b >TF) f -> _* ```

[``` # ```](#cmd--12)s `f` if `b` is falsy.


## CMD: [``` ?# ```](#cmd--20)

Stack: ``` a* (b >TF) f g -> _* ```

[``` # ```](#cmd--12)s `f` if `b` is truthy; else [``` # ```](#cmd--12)s `g`.


## CMD: [``` ??# ```](#cmd--21)

Stack: ``` a* (b >MAP) -> _* ```

Iterates through each key-value pair of `b`.
For each pair: if the [``` Q ```](#cmd-q-1) of the key is truthy,
then [``` # ```](#cmd--12)s the value and short-circuits.


## CMD: [``` *# ```](#cmd--22)

Stack: ``` a* f (n >NUM) -> _* ```

[``` # ```](#cmd--12)s `f` `n` times.


## CMD: [``` !# ```](#cmd--23)

Stack: ``` a* f g -> _* ```

Tries to [``` # ```](#cmd--12) `f`; on error, pushes caught `ERR` and [``` # ```](#cmd--12)s `g`.


## CMD: [``` !Q ```](#cmd-q-2)

Stack: ``` f' -> TRY' ```

[``` Q ```](#cmd-q-1)s `f` and wraps the result in a `TRY`.


## CMD: [``` ~Q ```](#cmd-q-3)

Stack: ``` f' -> TASK' ```

[``` Q ```](#cmd-q-1)s `f` asynchronously, returning a future.


## CMD: [``` >! ```](#cmd--24)

Stack: ``` (e ERR) -> ```

Throws `e`.


## CMD: [``` ' ```](#cmd--25)

Stack: ``` (a >ARR) f -> ARR ```

[``` # ```](#cmd--12)s `f` on `a` as if it were a stack.
```
[1 2 3 4] ( 5 swap ) '
-> [1 2 3 5 4]
```


## CMD: [``` '_ ```](#cmd-_)

Stack: ``` (a* >ARR) f -> _* ```

[``` # ```](#cmd--12)s `f` on the stack as if it were an `ARR`.
```
1 2 3 4 1.+.map '_
-> 2 3 4 5
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

Stack: ``` (a >NUM)' -> TF' ```

Whether `a` is an integer.


## CMD: [``` |_ ```](#cmd-_-1)

Stack: ``` (a >NUM)' -> NUM' ```

Rounds `a` towards -∞.


## CMD: [``` |~ ```](#cmd--26)

Stack: ``` (a >NUM)' -> NUM' ```

Rounds `a` to nearest integer.


## CMD: [``` |^ ```](#cmd--27)

Stack: ``` (a >NUM)' -> NUM' ```

Rounds `a` towards ∞.


## CMD: [``` X>b ```](#cmd-xb)

Stack: ``` (a >NUM)' (b >NUM)' -> ARR[NUM*]' ```

Converts `a` from decimal to `ARR` of base-`b` digits.
```
153 2X>b
-> [1 0 0 1 1 0 0 1]
```
```
153 16X>b
-> [9 9]
```


## CMD: [``` b>X ```](#cmd-bx)

Stack: ``` (a >ARR[>NUM*]) (b >NUM)' -> NUM' ```

Converts base-`b` digits to decimal.
```
[1 0 0 1 1 0 0 1] 2b>X
-> 153
```
```
[9 9] 16b>X
-> 153
```


## CMD: [``` >n/d ```](#cmd-nd-1)

Stack: ``` (a >NUM)' -> ARR[NUM NUM]' ```

Converts `a` to a numerator-denominator pair.
```
4 6/ >n/d
-> [2 3]
```
```
$PI >n/d
-> [68417829380157871863019543882359730131241 21778071482940061661655974875633165533184]
```


## CMD: [``` prec? ```](#cmd-prec)

Stack: ``` (a >NUM)' -> ARR[NUM NUM]' ```

Whether `a` is an exact value (i.e. represented in full precision).
```
2 3/ prec?
-> 1
```
```
$PI prec?
-> 0
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


## CMD: [``` + ```](#cmd--28)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a + b`


## CMD: [``` ++ ```](#cmd--29)

Stack: ``` (a >STR)' (b >STR)' -> STR' ```

Atomic [``` +` ```](#cmd--30).


## CMD: [``` +` ```](#cmd--30)

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
```
[1 2 3 4] 2-`
-> [1 3 4]
```
```
{0 1, 2 3, } 2-`
-> {0=>1}
```


## CMD: [``` * ```](#cmd--31)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a * b`


## CMD: [``` ** ```](#cmd--32)

Stack: ``` (a >STR)' (b >NUM)' -> STR' ```

Atomic [``` *` ```](#cmd--33).


## CMD: [``` *` ```](#cmd--33)

Stack: ``` a b -> _ ```

`a` replicated according to `b`.
If `b` is iterable, then `a` and `b` are recursively zipped together and replicated.
```
[1 2 3 4] [0 2 0 3] *` >A
-> [2 2 4 4 4]
```
```
[1 2 3 4] 3*` >A
-> [1 2 3 4 1 2 3 4 1 2 3 4]
```


## CMD: [``` / ```](#cmd--34)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a / b`. Throws error if `b` is 0.


## CMD: [``` /~ ```](#cmd--35)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

Integer [``` / ```](#cmd--34).


## CMD: [``` // ```](#cmd--36)

Stack: ``` (a >STR)' (b >NUM)' -> SEQ[STR*]' ```

Atomic [``` /` ```](#cmd--37).


## CMD: [``` /` ```](#cmd--37)

Stack: ``` a (b >NUM)' -> SEQ ```

`a` chunked to size `b`.
```
[1 2 3 4 5] 2/` >A
-> [[1 2] [3 4] [5]]
```


## CMD: [``` % ```](#cmd--38)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a (mod b)`


## CMD: [``` /% ```](#cmd--39)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' NUM' ```

Results of [``` /~ ```](#cmd--35) and [``` % ```](#cmd--38) on `a` and `b`.


## CMD: [``` %% ```](#cmd--40)

Stack: ``` (a >STR)' (b >NUM)' -> SEQ[STR*]' ```

Atomic [``` %` ```](#cmd--41).


## CMD: [``` %` ```](#cmd--41)

Stack: ``` a (b >NUM)' -> SEQ ```

`a` windowed to size `b`.
```
[1 2 3 4 5] 3%` >A
-> [[1 2 3] [2 3 4] [3 4 5]]
```


## CMD: [``` ^ ```](#cmd--42)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

`a ^ b`. Throws error if result would be a complex number.


## CMD: [``` ^~ ```](#cmd--43)

Stack: ``` (a >NUM)' (b >NUM)' -> NUM' ```

[``` ^ ```](#cmd--42) but `b` is coerced to `int`.


## CMD: [``` ^^ ```](#cmd--44)

Stack: ``` (a >STR)' (b >NUM)' -> SEQ[STR*]' ```

Atomic [``` ^` ```](#cmd--45).


## CMD: [``` ^` ```](#cmd--45)

Stack: ``` a (n >NUM)' -> SEQ' ```

Cartesian power of seed `a` to `n`.
```
"abc" 3^` >A
-> ["aaa" "aab" "aac" "aba" "abb" "abc" "aca" "acb" "acc" "baa" "bab" "bac" "bba" "bbb" "bbc" "bca" "bcb" "bcc" "caa" "cab" "cac" "cba" "cbb" "cbc" "cca" "ccb" "ccc"]
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
```
340P/
-> {2=>2 5=>1 17=>1}
```


## CMD: [``` ! ```](#cmd--46)

Stack: ``` (a >TF)' -> TF' ```

Atomic [``` !` ```](#cmd--47).


## CMD: [``` !` ```](#cmd--47)

Stack: ``` (a >TF) -> TF ```

Logical NOT.


## CMD: [``` & ```](#cmd--48)

Stack: ``` a' b' -> (a | b)' ```

Atomic [``` &` ```](#cmd--50).


## CMD: [``` && ```](#cmd--49)

Stack: ``` (a >TF)' (b >TF)' -> TF' ```

Atomic [``` &&` ```](#cmd--51).


## CMD: [``` &` ```](#cmd--50)

Stack: ``` a b -> a | b ```

Minimum of `a` and `b`.


## CMD: [``` &&` ```](#cmd--51)

Stack: ``` (a >TF) (b >TF) -> TF ```

Logical AND of `a` and `b`.


## CMD: [``` | ```](#cmd--52)

Stack: ``` a' b' -> (a | b)' ```

Atomic [``` |` ```](#cmd--54).


## CMD: [``` || ```](#cmd--53)

Stack: ``` (a >TF)' (b >TF)' -> TF' ```

Atomic [``` ||` ```](#cmd--55).


## CMD: [``` |` ```](#cmd--54)

Stack: ``` a b -> a | b ```

Maximum of `a` and `b`.


## CMD: [``` ||` ```](#cmd--55)

Stack: ``` (a >TF) (b >TF) -> TF ```

Logical OR of `a` and `b`.


## CMD: [``` <=> ```](#cmd--56)

Stack: ``` a' b' -> (-1 | 0 | 1)' ```

Atomic [``` <=>` ```](#cmd--57).


## CMD: [``` <=>` ```](#cmd--57)

Stack: ``` a b -> -1 | 0 | 1 ```

Comparison (-1, 0, or 1 depending on whether `a` is less than, equal to, or greater than `b`).


## CMD: [``` = ```](#cmd--58)

Stack: ``` a' b' -> TF' ```

Atomic [``` =` ```](#cmd--59).


## CMD: [``` =` ```](#cmd--59)

Stack: ``` a b -> TF ```

Whether `a` equals `b`.


## CMD: [``` != ```](#cmd--60)

Stack: ``` a' b' -> TF' ```

Atomic [``` !=` ```](#cmd--61).


## CMD: [``` !=` ```](#cmd--61)

Stack: ``` a b -> TF ```

Whether `a` does not equals `b`.


## CMD: [``` < ```](#cmd--62)

Stack: ``` a' b' -> TF' ```

Atomic [``` <` ```](#cmd--63).


## CMD: [``` <` ```](#cmd--63)

Stack: ``` a b -> TF ```

Whether `a` is less than `b`.


## CMD: [``` > ```](#cmd--64)

Stack: ``` a' b' -> TF' ```

Atomic [``` >` ```](#cmd--65).


## CMD: [``` >` ```](#cmd--65)

Stack: ``` a b -> TF ```

Whether `a` is greater than `b`.


## CMD: [``` <= ```](#cmd--66)

Stack: ``` a' b' -> TF' ```

Atomic [``` <=` ```](#cmd--67).


## CMD: [``` <=` ```](#cmd--67)

Stack: ``` a b -> TF ```

Whether `a` is less than or equal to `b`.


## CMD: [``` >= ```](#cmd--68)

Stack: ``` a' b' -> TF' ```

Atomic [``` >=` ```](#cmd--69).


## CMD: [``` >=` ```](#cmd--69)

Stack: ``` a b -> TF ```

Whether `a` is greater than or equal to `b`.


## CMD: [``` : ```](#cmd--70)

Stack: ``` a i' -> (a._ | UN)' ```

Value at atomic index `i` in `a`.


## CMD: [``` :r ```](#cmd-r)

Stack: ``` a -> a._ ```

Value at random index in `a`.


## CMD: [``` :` ```](#cmd--71)

Stack: ``` a i -> a._ | UN ```

Value at index `i` in `a`.


## CMD: [``` := ```](#cmd--72)

Stack: ``` a >ARR[i b] -> x ```

Sets value at index `i` in `a` to `b`.


## CMD: [``` :- ```](#cmd---2)

Stack: ``` a i -> x ```

Removes index `i` from `a`.


## CMD: [``` :? ```](#cmd--73)

Stack: ``` a b' -> TF' ```

Whether `a` has atomic `b`.


## CMD: [``` :?` ```](#cmd--74)

Stack: ``` a b -> TF ```

Whether `a` has `b`.
`MAP`s check `b` against keys; other types of `a` check `b` against values.


## CMD: [``` len ```](#cmd-len)

Stack: ``` a -> NUM ```

Length of `a`.


## CMD: [``` , ```](#cmd--75)

Stack: ``` a b -> ARR[a b] ```

Pairs `a` and `b` in an `ARR`.


## CMD: [``` ,, ```](#cmd--76)

Stack: ``` a -> ARR[a] ```

Wraps `a` in an `ARR`.


## CMD: [``` ,' ```](#cmd--77)

Stack: ``` a' b' -> ARR[a b]' ```

Vectorized [``` , ```](#cmd--75).


## CMD: [``` ,,' ```](#cmd--78)

Stack: ``` a' -> ARR[a]' ```

Vectorized [``` ,, ```](#cmd--76).


## CMD: [``` ,` ```](#cmd--79)

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


## CMD: [``` rflat ```](#cmd-rflat)

Stack: ``` a -> _ ```

Flattens `a` recursively.


## CMD: [``` rep ```](#cmd-rep)

Stack: ``` a -> SEQ ```

Infinite `SEQ` with `a` repeated.
```
5rep 10tk >A
-> [5 5 5 5 5 5 5 5 5 5]
```
```
[1 2 3] rep 10tk >A
-> [[1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3] [1 2 3]]
```


## CMD: [``` cyc ```](#cmd-cyc)

Stack: ``` a -> SEQ ```

Infinite `SEQ` with elements of `a` cycled.
```
[1 2 3] cyc 10tk >A
-> [1 2 3 1 2 3 1 2 3 1]
```


## CMD: [``` I* ```](#cmd-i-3)

Stack: ``` (a >NUM)' -> ARR[1*]' ```

Length-`a` `ARR` of 1's.
```
10I*
-> [1 1 1 1 1 1 1 1 1 1]
```


## CMD: [``` I^ ```](#cmd-i-4)

Stack: ``` (a >ARR) -> ARR ```

`ARR` of 1's with dimensions `a`.
```
[2 3 4] I^
-> [[[1 1 1 1] [1 1 1 1] [1 1 1 1]] [[1 1 1 1] [1 1 1 1] [1 1 1 1]]]
```


## CMD: [``` mold ```](#cmd-mold)

Stack: ``` a b -> _ ```

Convert the shape of `a` to the shape of `b`.
```
$W [2 3 4] I^ mold
-> [[[0 1 2 3] [4 5 6 7] [8 9 10 11]] [[12 13 14 15] [16 17 18 19] [20 21 22 23]]]
```
```
$W [1 2 3] I* mold
-> [[0] [1 2] [3 4 5]]
```


## CMD: [``` itr ```](#cmd-itr)

Stack: ``` a (f: b -> _) -> SEQ ```

Infinite `SEQ` of `f` successively [``` Q ```](#cmd-q-1)ed to `a`.
```
1 1.+ itr 10tk >A
-> [1 2 3 4 5 6 7 8 9 10]
```
```
1 ( 1+ 1 swap / ) itr 10tk >A
-> [1 1/2 2/3 3/5 5/8 8/13 13/21 21/34 34/55 55/89]
```


## CMD: [``` fold_ ```](#cmd-fold_)

Stack: ``` a (f: b -> _ _ | ) -> SEQ ```

`SEQ` generated from `f` successively [``` Q ```](#cmd-q-1)ed to `a`,
where `x` is the new current item and `y` is the next `b` to be subsequently [``` Q ```](#cmd-q-1)ed to `f`.
Generation stops if `f` [``` Q ```](#cmd-q-1)ed to `a` results in an empty stack.
```
0 1, ( ,_ tuck + dups \swap dip ) fold_ 10tk >A
-> [1 1 2 3 5 8 13 21 34 55]
```


## CMD: [``` >kv ```](#cmd-kv)

Stack: ``` a -> (SEQ | ARR)[ARR[k v]*] ```

`SEQ` of key/value pairs in `a`.
```
["a" "b" "c" "d"] >kv >A
-> [[0 "a"] [1 "b"] [2 "c"] [3 "d"]]
```
```
{"x""a", "y""b", "z""c", } >kv >A
-> [["x" "a"] ["y" "b"] ["z" "c"]]
```


## CMD: [``` =>kv ```](#cmd-kv-1)

Stack: ``` a -> MAP[(_ => _)*] ```

[``` >kv ```](#cmd-kv) and [``` >M ```](#cmd-m).
```
["a" "b" "c" "d"] =>kv
-> {0=>"a" 1=>"b" 2=>"c" 3=>"d"}
```


## CMD: [``` >k ```](#cmd-k)

Stack: ``` a -> SEQ | ARR ```

Keys in `a`.
```
{"x" "a", "y" "b", "z" "c", } >k >A
-> ["x" "y" "z"]
```


## CMD: [``` >v ```](#cmd-v)

Stack: ``` a -> SEQ | ARR ```

Values in `a`.
```
{"x""a", "y""b", "z""c", } >v >A
-> ["a" "b" "c"]
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
```
10O>a shuf
-> [4 9 6 1 8 0 7 5 2 3]
```


## CMD: [``` perm ```](#cmd-perm)

Stack: ``` a -> SEQ ```

All permutations of `a`.
```
[1 2 3] perm >A
-> [[1 2 3] [1 3 2] [2 1 3] [2 3 1] [3 1 2] [3 2 1]]
```


## CMD: [``` comb ```](#cmd-comb)

Stack: ``` a (n >NUM)' -> SEQ' ```

All length-`n` combinations of `a`.
```
[1 2 3] 2comb >A
-> [[1 2] [1 3] [2 3]]
```


## CMD: [``` ^set ```](#cmd-set)

Stack: ``` a -> SEQ ```

All subsets of `a`.
```
[1 2 3] ^set >A
-> [[] [1] [2] [3] [1 2] [1 3] [2 3] [1 2 3]]
```


## CMD: [``` Q* ```](#cmd-q-4)

Stack: ``` a[_*] -> SEQ' ```

Cartesian product of iterable-of-iterables `a` to `n`.
```
["abc" "def" "ghi"] Q* >A
-> ["adg" "adh" "adi" "aeg" "aeh" "aei" "afg" "afh" "afi" "bdg" "bdh" "bdi" "beg" "beh" "bei" "bfg" "bfh" "bfi" "cdg" "cdh" "cdi" "ceg" "ceh" "cei" "cfg" "cfh" "cfi"]
```


## CMD: [``` tpose ```](#cmd-tpose)

Stack: ``` a[_*] -> _[_*] ```

Transposes a collection of collections matrix-style.
```
[[1 2 3][4 5 6][7 8 9]] tpose
-> [[1 4 7] [2 5 8] [3 6 9]]
```
```
[[1 2][3 4 5][6]] tpose
-> [[1 3 6] [2 4] [5]]
```


## CMD: [``` S>c ```](#cmd-sc)

Stack: ``` (a >STR)' -> ARR[NUM*]' ```

Converts `a` to codepoints.
```
"hello"S>c
-> [104 101 108 108 111]
```


## CMD: [``` c>S ```](#cmd-cs)

Stack: ``` (a >ARR[NUM*]) -> STR ```

Converts iterable of codepoints to `STR`.
```
[104 101 108 108 111] c>S
-> "hello"
```


## CMD: [``` <> ```](#cmd--80)

Stack: ``` (a >STR)' (b >STR)' -> ARR' ```

Splits `a` with `b`.


## CMD: [``` <>: ```](#cmd--81)

Stack: ``` a (i >NUM) -> ARR[_ _] ```

[``` tk ```](#cmd-tk) and [``` dp ```](#cmd-dp) of `a` at index `i`.


## CMD: [``` c<> ```](#cmd-c)

Stack: ``` (a >STR)' -> ARR' ```

[``` <> ```](#cmd--80)s with empty string.


## CMD: [``` w<> ```](#cmd-w-1)

Stack: ``` (a >STR)' -> ARR' ```

[``` <> ```](#cmd--80)s with space.


## CMD: [``` n<> ```](#cmd-n-3)

Stack: ``` (a >STR)' -> ARR' ```

[``` <> ```](#cmd--80)s with newline.


## CMD: [``` s<> ```](#cmd-s-1)

Stack: ``` (a >STR)' -> ARR' ```

[``` <> ```](#cmd--80)s on whitespace characters.


## CMD: [``` >< ```](#cmd--82)

Stack: ``` a (b >STR)' -> STR' ```

Joins `a` with `b`.


## CMD: [``` c>< ```](#cmd-c-1)

Stack: ``` a -> STR' ```

[``` >< ```](#cmd--82)s with empty string.


## CMD: [``` w>< ```](#cmd-w-2)

Stack: ``` a -> STR' ```

[``` >< ```](#cmd--82)s with space.


## CMD: [``` n>< ```](#cmd-n-4)

Stack: ``` a -> STR' ```

[``` >< ```](#cmd--82)s with newline.


## CMD: [``` A>a ```](#cmd-aa)

Stack: ``` (a >STR)' -> STR' ```

Converts `STR` to `lowercase`.


## CMD: [``` a>A ```](#cmd-aa-1)

Stack: ``` (a >STR)' -> STR' ```

Converts `STR` to `UPPERCASE`.


## CMD: [``` >Aa ```](#cmd-aa-2)

Stack: ``` (a >STR)' -> STR' ```

Converts `STR` to `Capitalized`.


## CMD: [``` /? ```](#cmd--83)

Stack: ``` (a >STR)' (r >STR)' -> SEQ[MAP]' ```

Matches `a` with regex `r`.
Each match returned is a `MAP` with the following keys:
- ``` & ```: Matched `STR`.
- ``` ` ```: `STR` before the match.
- ``` ' ```: `STR` after the match.
- ``` * ```: `ARR[MAP]` of each capturing group matched.
- ``` ^ ```: `NUM` index of the match's start.
- ``` $ ```: `NUM` index of the match's end.


## CMD: [``` /?& ```](#cmd--84)

Stack: ``` (a >STR)' (r >STR)' -> SEQ[STR]' ```

[``` /? ```](#cmd--83) with only `&` keys.


## CMD: [``` /?` ```](#cmd--85)

Stack: ``` (a >STR)' (r >STR)' -> SEQ[STR]' ```

[``` /? ```](#cmd--83) with only `'` keys.


## CMD: [``` /?' ```](#cmd--86)

Stack: ``` (a >STR)' (r >STR)' -> SEQ[STR]' ```

[``` /? ```](#cmd--83) with only ``` ` ``` keys.


## CMD: [``` /?* ```](#cmd--87)

Stack: ``` (a >STR)' (r >STR)' -> SEQ[ARR[MAP]]' ```

[``` /? ```](#cmd--83) with only `*` keys.


## CMD: [``` /?^ ```](#cmd--88)

Stack: ``` (a >STR)' (b >STR)' -> SEQ[NUM]' ```

[``` /? ```](#cmd--83) with only `^` keys.


## CMD: [``` /?$ ```](#cmd--89)

Stack: ``` (a >STR)' (b >STR)' -> SEQ[NUM]' ```

[``` /? ```](#cmd--83) with only `$` keys.


## CMD: [``` /# ```](#cmd--90)

Stack: ``` (a >STR)' (r >STR)' (f: MAP -> >STR)' -> STR' ```

Replace matches of regex `r` on `a` by applying each match `MAP` to `f`.


## CMD: [``` /#^ ```](#cmd--91)

Stack: ``` (a >STR)' (r >STR)' (s >STR)' -> STR' ```

Replace first match of regex `r` on `a` with `s`.


## CMD: [``` map ```](#cmd-map)

Stack: ``` a f' -> _' ```

[``` Q ```](#cmd-q-1)s `f` on each element of `a`.
If `a` is `MAP`, then the signature of `f` is `k v -> _`,
where `k=>v` is the key-value pair.
Otherwise, the signature of `f` is `x -> _`,
where `x` is the element.
```
[1 2 3 4] 1.+ map
-> [2 3 4 5]
```
```
{0 1, 2 3, 4 5, } ( over + ) map
-> {0=>1 2=>5 4=>9}
```


## CMD: [``` tap ```](#cmd-tap)

Stack: ``` a f' -> a ```

[``` map ```](#cmd-map) but `a` is preserved (i.e. leaving only side effects of `f`).
```
[1 2 3 4] ( 1+ n>o ) tap
-> [1 2 3 4]
2
3
4
5
```


## CMD: [``` zip ```](#cmd-zip)

Stack: ``` a b (f: x y -> _)' -> _' ```

[``` Q ```](#cmd-q-1)s `f` over each element-wise pair of `a` and `b`.
Iterables of differing length truncate to the shorter length when zipped.
```
[1 2 3 4] [2 3 4 5] \, zip
-> [[1 2] [2 3] [3 4] [4 5]]
```
```
[1 2 3 4] [2 3] \+ zip
-> [3 5]
```
```
[1 2 3 4] {1 "a", 3 "b", "x" "c", } \, zip
-> [[1 [1 "a"]] [2 [3 "b"]] [3 ["x" "c"]]]
```


## CMD: [``` zip~ ```](#cmd-zip-1)

Stack: ``` a b c d (f: x y -> _)' -> _' ```

[``` zip ```](#cmd-zip) but instead of truncating,
uses `c` and `d` as fill elements for `a` and `b` respectively.
```
[1 2 3 4] [2 3 4 5] UN UN \, zip~
-> [[1 2] [2 3] [3 4] [4 5]]
```
```
[1 2 3 4] [2 3] UN UN \+ zip~
-> [3 5 3 4]
```
```
[1 2 3 4] {1 "a", 3 "b", "x" "c", } UN UN \, zip~
-> [[1 [1 "a"]] [2 [3 "b"]] [3 ["x" "c"]] [4 UN]]
```


## CMD: [``` tbl ```](#cmd-tbl)

Stack: ``` a b (f: x y -> _)' -> _' ```

[``` Q ```](#cmd-q-1)s `f` over each table-wise pair of `a` and `b`.
```
[1 2 3 4] [2 3 4 5] \++ tbl
-> [["12" "13" "14" "15"] ["22" "23" "24" "25"] ["32" "33" "34" "35"] ["42" "43" "44" "45"]]
```


## CMD: [``` mapf ```](#cmd-mapf)

Stack: ``` a f' -> _' ```

[``` map ```](#cmd-map) and [``` flat ```](#cmd-flat).
```
1224P/ \*` mapf
-> [2 2 2 3 3 17]
```


## CMD: [``` rmap ```](#cmd-rmap)

Stack: ``` a f' -> _' ```

Atomic/recursive [``` map ```](#cmd-map).
```
[[1 2] 3 4 [5 [6 7]]] ( dup n>o ) rmap
-> [[1 2] 3 4 [5 [6 7]]]
1
2
3
4
5
6
7
```


## CMD: [``` fold ```](#cmd-fold)

Stack: ``` a b f' -> _' ```

[``` Q ```](#cmd-q-1)s `f` to combine each accumulator and element starting from initial accumulator `b`.
If `a` is `MAP`, then the signature of `f` is `k x v -> _`,
where `k=>v` is the key-value pair and `x` is the accumulator.
Otherwise, the signature of `f` is `x y -> _`,
where `x` is the accumulator and `y` is the value.
```
[1 2 3 4] 0 \+ fold
-> 10
```
```
"1011"_` =>kv 0 ( rot 2 swap ^ * + ) fold
-> 0
```


## CMD: [``` rfold ```](#cmd-rfold)

Stack: ``` a b f' -> _' ```

Atomic/recursive [``` fold ```](#cmd-fold).
```
[[1 2] 3 4 [5 [6 7]]] 0 \+ rfold
-> 28
```
```
[[1 2] 3 4 [5 [6 7]]] [] \+` rfold
-> [1 2 3 4 5 6 7]
```


## CMD: [``` fold~ ```](#cmd-fold-1)

Stack: ``` a f' -> _' ```

[``` fold ```](#cmd-fold) without initial accumulator, instead using the first element of `a`.
If `a` is empty, then an error is thrown.
```
[1 2 3 4] \+ fold~
-> 10
```
```
[1 5 10 4 3] \| fold~
-> 10
```


## CMD: [``` scan ```](#cmd-scan)

Stack: ``` a b f' -> _' ```

[``` fold ```](#cmd-fold) with intermediate values.
```
[1 2 3 4] 0 \+ scan
-> [0 1 3 6 10]
```


## CMD: [``` +/ ```](#cmd--92)

Stack: ``` a -> NUM' ```

Sum of `a`. Equivalent to `0 \+ rfold`.


## CMD: [``` */ ```](#cmd--93)

Stack: ``` a -> NUM' ```

Product of `a`. Equivalent to `1 \* rfold`.


## CMD: [``` walk ```](#cmd-walk)

Stack: ``` a f' -> _' ```

A multi-purpose function for creating, modifying, and traversing nested structures.
```
[[1 2] 3 4 { "a" 5, "b" [6 7] , }] ( dups f>o ) walk
-> [[1 2] 3 4 {"a"=>5 "b"=>[6 7]}]
[[[1 2] 3 4 {"a"=>5 "b"=>[6 7]}]]
[[1 2]]
[1]
[2]
[3]
[4]
[{"a"=>5 "b"=>[6 7]}]
["a" 5]
["b" [6 7]]
[6]
[7]
```
```
[[1 2] 3 4 { "a" 5, "b" [6 7] , }] ( dup len ( dup +` ) &# ) walk
-> [[1 2 1 2] 3 4 {"a"=>5 "b"=>[6 7 6 7]} [1 2 1 2] 3 4 {"a"=>5 "b"=>[6 7 6 7]}]
```


## CMD: [``` fltr ```](#cmd-fltr)

Stack: ``` a f' -> _' ```

Keeps elements of `a` that satisfy predicate `f`.
If `a` is `MAP`, then the signature of `f` is `k v -> >TF`,
where `k=>v` is the key-value pair.
Otherwise, the signature of `f` is `x -> >TF`,
where `x` is the element.
```
[5 1 2 4 3] 2.> fltr
-> [5 4 3]
```


## CMD: [``` any ```](#cmd-any)

Stack: ``` a f' -> TF' ```

Whether any elements of `a` satisfy predicate `f`.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```
[5 1 2 4 3] 2.> any
-> $T
```


## CMD: [``` all ```](#cmd-all)

Stack: ``` a f' -> TF' ```

Whether all elements of `a` satisfy predicate `f`.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```
[5 1 2 4 3] 2.> all
-> $F
```


## CMD: [``` tk* ```](#cmd-tk-1)

Stack: ``` a f' -> _' ```

Takes elements of `a` until [``` Q ```](#cmd-q-1)ing `f` is falsy.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```
[5 1 2 4 3] 4.!= tk*
-> [5 1 2]
```


## CMD: [``` dp* ```](#cmd-dp-1)

Stack: ``` a f' -> _' ```

Drops elements of `a` while predicate `f` is truthy.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```
[5 1 2 4 3] 4.!= dp*
-> [4 3]
```


## CMD: [``` find ```](#cmd-find)

Stack: ``` a f' -> _' ```

Finds first element of `a` where predicate `f` is truthy.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
Returns `UN` if not found.
```
[5 1 2 4 3] ( 2% ! ) find
-> 2
```


## CMD: [``` find: ```](#cmd-find-1)

Stack: ``` a f' -> NUM' ```

Finds index of first element of `a` where predicate `f` is truthy.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
Returns `-1` if not found.
```
[5 1 2 4 3] ( 2% ! ) find:
-> 2
```


## CMD: [``` uniq ```](#cmd-uniq)

Stack: ``` a f' -> _' ```

Uniquifies elements of `a` with mapper `f`.
See [``` map ```](#cmd-map) for the signature of `f`.
```
[5 1 2 4 3] 3.% uniq
-> [5 1 3]
```


## CMD: [``` sort ```](#cmd-sort)

Stack: ``` a f' -> _' ```

Sorts elements of `a` with mapper `f`.
See [``` map ```](#cmd-map) for the signature of `f`.
```
["a" "" "abc" "ab"] \len sort
-> ["" "a" "ab" "abc"]
```
```
[1 2 3 4 5] \$rng sort
-> [4 3 2 1 5]
```


## CMD: [``` sort~ ```](#cmd-sort-1)

Stack: ``` a f' -> _' ```

Sorts elements of `a` with comparator `f`.
If `a` is `MAP`, then the signature of `f` is `ARR[k v] ARR[j w] -> >TF`,
where `k=>v` and `j=>w` are key-value pairs to compare.
Otherwise, the signature of `f` is `x y -> >TF`,
where `x` and `y` are elements to compare.
```
[1 5 2 3 4] \< sort~
-> [1 2 3 4 5]
```
```
[1 5 2 3 4] \> sort~
-> [5 4 3 2 1]
```


## CMD: [``` part ```](#cmd-part)

Stack: ``` a f' -> ARR[_ _]' ```

Separates `a` into 2 parts based on predicate `f`.
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```
[5 1 2 4 3] 2.> part
-> [[5 4 3] [1 2]]
```


## CMD: [``` group ```](#cmd-group)

Stack: ``` a f' -> MAP' ```

Separates `a` groups based on `f`.
Each result of `f` becomes a key in the resulting `MAP`.
See [``` map ```](#cmd-map) for the signature of `f`.
```
"abc"^set >A \len group
-> {0=>[""] 2=>["ab" "ac" "bc"] 1=>["a" "b" "c"] 3=>["abc"]}
```


## CMD: [``` span ```](#cmd-span)

Stack: ``` a f' -> ARR[_ _]' ```

Equivalent to a combination of [``` tk* ```](#cmd-tk-1) and [``` dp* ```](#cmd-dp-1).
See [``` fltr ```](#cmd-fltr) for the signature of `f`.
```
[5 1 2 4 3] 2.% span
-> [[5 1] [2 4 3]]
```


## CMD: [``` pack ```](#cmd-pack)

Stack: ``` a f' -> _' ```

Groups consecutive duplicate runs of `a` based on predicate `f`.
See [``` sort~ ```](#cmd-sort-1) for the signature of `f`.
```
[1 1 2 3 3 4 6 4 4] \=` pack
-> [[1 1] [2] [3 3] [4] [6] [4 4]]
```


## CMD: [``` ~_ ```](#cmd-_-6)

Stack: ``` (a >FUT[x])' -> x' ```

Synchronously waits for `a` to complete, leaving the result on the stack.


## CMD: [``` ~_! ```](#cmd-_-7)

Stack: ``` (a >FUT[x])' -> TRY[x]' ```

[``` ~_ ```](#cmd-_-6) with result wrapped in a `TRY`.


## CMD: [``` ~$ ```](#cmd--94)

Stack: ``` (a >FUT)' -> ```

Cancels `a`.


## CMD: [``` ~|> ```](#cmd--95)

Stack: ``` a[>TASK*] -> TASK[_[_*]] ```

Executes each `TASK` in `a` sequentially such that both effects and results are ordered.


## CMD: [``` ~|| ```](#cmd--96)

Stack: ``` a[>TASK*] -> TASK[_[_*]] ```

Executes each `TASK` in `a` in parallel such that effects are unordered but results are ordered.


## CMD: [``` ~||> ```](#cmd--97)

Stack: ``` a[>TASK*] (n >NUM) -> TASK[_[_*]] ```

[``` ~|| ```](#cmd--96) but with at most `n` concurrently running `TASK`s.


## CMD: [``` ~// ```](#cmd--98)

Stack: ``` a[>TASK*] -> TASK[_[_*]] ```

[``` ~|| ```](#cmd--96) but results are also unordered.


## CMD: [``` ~>> ```](#cmd--99)

Stack: ``` a[>TASK*] -> TASK ```

Races a collection of `TASK`s, returning the first to complete.


## CMD: [``` ~< ```](#cmd--100)

Stack: ``` (a >TASK)' -> TASK' ```

Ensures that `a` runs on a separate thread.


## CMD: [``` ~: ```](#cmd--101)

Stack: ``` (a >TASK)' -> TASK' ```

Ensures that `a` is memoized such that subsequent runs of the task return the same value.


## CMD: [``` ~:& ```](#cmd--102)

Stack: ``` (a >TASK)' -> TASK' ```

[``` ~: ```](#cmd--101) but only if `a` completes successfully.


## CMD: [``` ~$_ ```](#cmd-_-8)

Stack: ``` (a >TASK)' -> TASK' ```

Ensures that `a` is uncancellable.


## CMD: [``` ~% ```](#cmd--103)

Stack: ``` (a >TASK)' (n >NUM)' -> TASK' ```

Ensures that `a` will error if not completed within `n` milliseconds.


## CMD: [``` sleep ```](#cmd-sleep)

Stack: ``` (n >NUM)' -> TASK[n]' ```

Creates an asynchronous `TASK` that will complete after `n` milliseconds.

