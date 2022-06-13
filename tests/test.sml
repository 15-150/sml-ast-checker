(* fun foo k 0 = k 1
  | foo k x = foo (fn y => k (y + x)) (x - 1)

fun foo x y = op+ (op+ (x, 1), y) *)

fun f x = f (x - 1)

fun g x = List.length x

fun fact 0 = 1
  | fact n = n * fact (n - 1)

fun ahh k x = foo k x
