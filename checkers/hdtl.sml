structure HdTl : CHECK =
  struct
    open Ast

    fun is_sym sym item =
      case item of
        MarkExp (exp, _) => is_sym sym exp
      | SeqExp [exp] => is_sym sym exp
      | VarExp [] => false
      | VarExp syms => Symbol.name (List.last syms) = sym
      | FlatAppExp [s] => is_sym sym (# item s)
      | _ => false

    fun chk exp = (is_sym "hd" exp) orelse (is_sym "tl" exp)

    fun find_hdtl exp =
      case exp of
        AppExp {function, ...} => if chk function then [(exp, NONE)] else []
      | FlatAppExp (L as _ :: _ :: _) =>
          if List.exists (fn {item, ...} => chk item) L then
            [(exp, NONE)]
          else
            []
      | _ => []

    val check = find_hdtl

    val warning = "checked for hd/tl _"

    val hint = "use pattern matching instead"

  end
