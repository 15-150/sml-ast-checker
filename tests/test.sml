fun foo k 0 = k 1
  | foo k x = foo (fn y => k (y + x)) (x - 1)

fun foo x y = op+ (op+ (x, 1), y)
(* fun bar x y = x + 1 + y *)

(* fun f x = f (x - 1) *)

(* fun g x = List.length x *)

fun fact 0 = 1
  | fact n = n * fact (n - 1)

fun factCPS 0 k = k 1
  | factCPS n k = factCPS (n - 1) (fn res => k (n * res))

(* fun ahh k x = foo k x *)
