structure Types =
  struct
    open Ast

    fun regionify region (elem, NONE) = (elem, SOME region)
      | regionify _ (elem, SOME region) = (elem, SOME region)

    fun check (d : dec) =
      case d of
        DatatypeDec _ => [(d, NONE)]
      | TypeDec _ => [(d, NONE)]
      | DataReplDec _ => [(d, NONE)]
      (* SML/NJ: `datatype x = datatype y` *)
      | _ => []

    val warning = "(data)type declarations are forbidden"
    val hint = ""
  end
