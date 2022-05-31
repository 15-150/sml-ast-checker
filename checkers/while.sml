structure While : CHECK =
  struct
    open Ast

    fun check e =
      case e of
        WhileExp _ => [(e, NONE)]
      | _ => []

    val warning = "using a while loop"
    val hint = ""
  end
