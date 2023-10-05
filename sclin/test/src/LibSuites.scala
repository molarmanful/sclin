package sclin

import ANY._

class CmpSuite extends TU:

  "UN = UN" |? "UN dup =`" ==> TF(true)
  "UN < _" |? "UN $F <`" ==> TF(true)
  "$F = $F" |? "$F dup =`" ==> TF(true)
  "$F < _" |? "$F 1D0/ _ <`" ==> TF(true)
  "neg < empty itr" |? "1_ [] <`" ==> TF(true)
  "neg < empty STR" |? "1_ \"\" <`" ==> TF(true)
  "NUM vs. DBL" |? "12340913284129341023948123 1D0/ <`" ==> TF(true)
  "STR vs. NUM" |? "\"a\" 97=`" ==> TF(true)
  "itr vs. itr 0" |? "[1 2 3] [1 2 3 4] <`" ==> TF(true)
  "itr vs. itr 1" |? "[2 2 3] [1 2 3 4] >`" ==> TF(true)
  "itr vs. NUM" |? "[1 2 3] 2<`" ==> TF(true)
  "itr vs. STR" |? "[97 98 99] \"abc\"=`" ==> TF(true)
  "_ < $T" |? "1D0/ $T <`" ==> TF(true)
  "$T = $T" |? "$T dup =`" ==> TF(true)
