
fun fact 0 = 1
  | fact n = n * fact (n - 1)
fun factCPS 0 k = k 1
  | factCPS n k = factCPS (n - 1) (fn res => k (n * res))

val test1 = 1

fun test2 x = test1 + 1

fun test3 x =
  let
    val zero = 0
  in
    zero
  end

fun test4 x =
  let
    val fact4 = fact 4
  in
    fact4
  end

fun test5 x =
  let
    val fact5 = factCPS 5 SOME
  in
    fact5
  end

fun test6 x =
  let
    val fact6 = factCPS 6 SOME
  in
    test6 (fact6)
  end

fun test7 (a,b) =
  let
    val (a,b) = (a,b)
  in
    (a,b)
  end
