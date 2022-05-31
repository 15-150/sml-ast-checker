structure Modules =
  struct
    open Ast

    fun regionify region (elem, NONE) = (elem, SOME region)
      | regionify _ (elem, SOME region) = (elem, SOME region)

    fun check (d : dec) =
      case d of
        FctDec _ => [(d, NONE)]
      | FsigDec _ => [(d, NONE)]
      | SigDec _ => [(d, NONE)]
      | StrDec _ => [(d, NONE)]
      | _ => []

    val warning = "modules are forbidden"
    val hint = ""
  end
