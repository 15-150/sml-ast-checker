structure Exceptions =
  struct
    open Ast

    fun check (d : dec) =
      case d of
        ExceptionDec _ => [(d, NONE)]
      | _ => []

    val warning = "exception declarations are forbidden"
    val hint = ""
  end
