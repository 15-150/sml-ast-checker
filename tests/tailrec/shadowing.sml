(* Shadowing test 1 *)
fun fact1 0 = 1
  | fact1 x = x * fact1 (x - 1)

fun fact1 k 0 = k 1
  | fact1 k x = fact1 (fn res => k (x * res)) (x - 1)

fun fact2 x = fact1 (fn x => x) x

(* Shadowing test 2 *)
fun fact3 k 0 = k 1
  | fact3 k x = fact3 (fn res => k (x * res)) (x - 1)

fun fact3 0 = 1
  | fact3 x = x * fact3 (x - 1)

fun fact4 x = fact3 x

(* Shadowing and let *)
fun fact5 x =
  let
    fun innerFact k 0 = k 1
      | innerFact k x = innerFact (fn res => k (x * res)) (x - 1)

    fun innerFact 0 = 1
      | innerFact x = x * innerFact (x - 1)
  in
    innerFact (fn x => x) x
  end

fun fact6 x =
  let
    fun innerFact 0 = 1
      | innerFact x = x * innerFact (x - 1)

    fun innerFact k 0 = k 1
      | innerFact k x = innerFact (fn res => k (x * res)) (x - 1)
  in
    innerFact (fn x => x) x
  end

(* Shadowing with bound variables *)
fun f xs = List.length xs
fun fold1 f xs = List.foldl f 0 xs
fun useFold1 xs = fold1 f xs
