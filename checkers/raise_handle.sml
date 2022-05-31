(* Used for exceptions/universal where they shouldn't be raising/handling anything
 * Probably don't want to use this as a general checker.
 *)
structure RaiseHandle : CHECK =
  struct
    open Ast

    fun check e =
      case e of
        HandleExp _ => [(e, NONE)]
      | RaiseExp _ => [(e, NONE)]
      | _ => []

    val warning = "raise/handle expressions are forbidden"
    val hint = ""
  end
