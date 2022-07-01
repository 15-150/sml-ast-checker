
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

fun sapling x = Node (Empty, x, Empty)
