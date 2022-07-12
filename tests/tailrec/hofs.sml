fun f xs =
  let
    fun id x = x
  in
    f (List.map id xs)
  end

fun g k xs =
  let
    fun id x = x
  in
    f (fn res => k (List.map id xs)) xs
  end

fun fold1 f xs = List.foldl f 0 xs

fun fold2 f xs = List.foldr f 0 xs

fun evens xs = List.filter (fn x => x % 2 == 0) xs

fun sumAll xs = List.foldl (fn (x, y) => x + y) 0 xs

fun apSumAll () = sumAll [1, 2, 3]
