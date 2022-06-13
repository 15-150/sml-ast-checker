(* currently can only test one of these at a time so just comment out all the
 * ones you don't want
 *)

fun f x = [] @ x + x * x :: []
fun g x = 1::2::3 + 3::[]
fun h x = x before print "hi"
val val_restrict = Fn.id o Fn.id

(* nonfix tests *)

fun add a b = op+ (a, b)
fun k f x = f (f x)

