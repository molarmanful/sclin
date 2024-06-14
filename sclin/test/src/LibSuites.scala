package sclin

import ANY.*

class CmpSuite extends TU:

  "UN = UN" |?| "UN dup =`"
  "UN < _" |?| "UN $F <`"
  "$F = $F" |?| "$F dup =`"
  "$F < _" |?| "$F 1D0/ _ <`"
  "neg < empty itr" |?| "1_ [] <`"
  "neg < empty STR" |?| "1_ \"\" <`"
  "NUM vs. DBL" |?| "12340913284129341023948123 1D0/ <`"
  "STR vs. NUM" |?| "\"a\" 97=`"
  "itr vs. itr 0" |?| "[1 2 3] [1 2 3 4] <`"
  "itr vs. itr 1" |?| "[2 2 3] [1 2 3 4] >`"
  "itr vs. NUM" |?| "[1 2 3] 2<`"
  "itr vs. STR" |?| "[97 98 99] \"abc\"=`"
  "_ < $T" |?| "1D0/ $T <`"
  "$T = $T" |?| "$T dup =`"

class IGetSuite extends TU

class ISetSuite extends TU

class IModSuite extends TU

class IRemoveSuite extends TU

class ConcatSuite extends TU:

  "UN ++ UN" |? "UN UN +`" ==> UN
  "UN ++ _" |? "UN [1 2 3] +`" ==> dARR(NUM(1), NUM(2), NUM(3))
  "ARR ++ ARR" |? "[1 2 3] [4 5 6] +`" ==> dARR(
    NUM(1), NUM(2), NUM(3), NUM(4), NUM(5), NUM(6)
  )
  "MAP ++ MAP" |? "[\"a\" 1, \"b\" 2, ]: [\"c\" 3, \"d\" 4, ]: +`" ==> dMAP(
    STR("a") -> NUM(1),
    STR("b") -> NUM(2),
    STR("c") -> NUM(3),
    STR("d") -> NUM(4)
  )
  "STR ++ STR" |? "\"abc\" \"def\"+`" ==> STR("abcdef")
  "itr ++ itr" |? "(1 2 3) [4 5 6] +`" ==> dFN(
    0, NUM(1), NUM(2), NUM(3), NUM(4), NUM(5), NUM(6)
  )
  "_ ++ _" |? "1 2+`" ==> dARR(NUM(1), NUM(2))

class TakeSuite extends TU:

  "take 0" |? "[1 2 3] 0tk" ==> dARR()
  "take n" |? "[1 2 3] 2tk" ==> dARR(NUM(1), NUM(2))
  "take -n" |? "[1 2 3] 2_ tk" ==> dARR(NUM(2), NUM(3))

class DropSuite extends TU:

  "drop 0" |? "[1 2 3] 0dp" ==> dARR(NUM(1), NUM(2), NUM(3))
  "drop n" |? "[1 2 3] 2dp" ==> dARR(NUM(3))
  "drop -n" |? "[1 2 3] 2_ dp" ==> dARR(NUM(1))
