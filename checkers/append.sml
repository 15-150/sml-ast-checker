structure Append : CHECK =
  struct
    open Ast

    fun find_append e =
      case e of
        MarkExp (e, r) => find_append e
      | VarExp [sym] => Symbol.name sym = "@"
      | FlatAppExp [{fixity, item, region}] => find_append item
      | _ => false

    fun find_singleton e =
      case e of
        MarkExp (e, r) => find_singleton e
      | FlatAppExp [{fixity, item, region}] => find_singleton item
      | ListExp [s] => true
      | _ => false

    fun pairs f g [] = false
      | pairs f g [x] = false
      | pairs f g (x :: y :: xs) =
        if f x andalso g y then
          true
        else
          pairs f g (y :: xs)

    fun find_append_singleton e =
      case e of
        FlatAppExp xs =>
          if
            pairs (fn x => find_singleton (# item x))
              (fn y => find_append (# item y))
              xs
          then
            [(e, NONE)]
          else
            []
      | _ => []

    val check = find_append_singleton
    val warning = "appended singleton to the front of list"
    val hint = "use :: instead"
  end
