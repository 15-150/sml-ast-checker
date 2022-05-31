
exception Test

exception Test2 of int

exception Test3 = Div

fun f x =
  let
    exception Test4 of 'a * 'b
  in
    ()
  end

fun ('a, 'b) f x =
  let
    exception Test5 of 'a * 'b
  in
    ()
  end
