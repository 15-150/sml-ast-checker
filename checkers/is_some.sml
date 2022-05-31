structure IsSomeOption : CHECK =
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

    fun find_is_some exp =
      case exp of
        AppExp {function, ...} =>
          if is_sym "isSome" function then
            [(exp, NONE)]
          else
            []
      | FlatAppExp (L as _ :: _ :: _) =>
          if List.exists (fn {item, ...} => is_sym "isSome" item) L then
            [(exp, NONE)]
          else
            []
      | _ => []

    val check = find_is_some

    val warning = "checked for SOME _ using isSome"

    val hint = "use pattern matching instead"

  end
