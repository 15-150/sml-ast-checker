structure EqualsOption : CHECK =
struct
  open Ast

  fun is_sym sym item =
      case item of
          MarkExp (exp, _) => is_sym sym exp
        | SeqExp [exp] => is_sym sym exp
        | VarExp [] => false
        | VarExp syms => Symbol.name (List.last syms) = sym
        | FlatAppExp [s] => is_sym sym (#item s)
        | _ => false

  fun find_pair f [] = false
    | find_pair f [x] = false
    | find_pair f (x::y::xs) =
      if f (x, y) orelse f (y, x)
      then true
      else find_pair f (y::xs)

  fun find_option_check exp =
    case exp of
      FlatAppExp (L as _::_::_) =>
        if
          find_pair
            (fn (x, y) => (is_sym "=" (#item x) orelse is_sym "<>" (#item x))
                            andalso is_sym "NONE" (#item y))
            L
        then [(exp, NONE)]
        else []
    | _ => []

  fun check exp = find_option_check exp

  val warning = "checked for NONE using = or <>"

  val hint = "use pattern matching instead"

end
