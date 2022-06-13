(* fun foo k 0 = k 1
  | foo k x = foo (fn y => k (y + x)) (x - 1)

fun foo x y = op+ (op+ (x, 1), y) *)

fun f (x : int) : int = f (x - 1)

(* fun g x = List.length x *)