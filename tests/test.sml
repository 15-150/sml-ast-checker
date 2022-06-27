fun foo k 0 = k 1
  | foo k x = foo (fn y => k (y + x)) (x - 1)

fun foo x y = op+ (op+ (x, 1), y)
fun bar x y = x + 1 + y

fun f x = f (x - 1)

fun g x = List.length x

fun eta k x = k x

fun fact 0 = 1
  | fact n = n * fact (n - 1)

fun factCPS 0 k = k 1
  | factCPS n k = factCPS (n - 1) (fn res => k (n * res))

fun iszero 0 = NONE
  | iszero n = SOME n

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

fun leftmost Empty = NONE
  | leftmost (Node (l,x,r)) =
    case leftmost l of
      NONE => SOME x
    | opt  => opt

fun leftmost_t Empty acc = acc
  | leftmost_t (Node (l,x,r)) acc = leftmost_t l (SOME x)

fun treefind p t sc fc =
  case t of
    Empty => fc ()
  | Node (l,x,r) =>
    if p x
    then sc x
    else treefind p l sc (fn () => treefind p r sc fc)

(* fun ahh k x = foo k x *)

(* fixity tests *)
infix 5 ++ --
fun x ++ y = x + y
fun op-- (x,y) = x - y

nonfix +
fun + x y = + x y
