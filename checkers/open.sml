(* This is specific to 15-150, since open clutters the namespace *)
structure Open =
  struct
    open Ast

    fun regionify region (elem, NONE) = (elem, SOME region)
      | regionify _ (elem, SOME region) = (elem, SOME region)

    fun check_strexp exp =
      case exp of
        AppStr _ => []
      | AppStrI _ => []
      | BaseStr d => check d
      | ConstrainedStr (strexp, _) => check_strexp strexp
      | LetStr (dec, strexp) => check dec @ check_strexp strexp
      | MarkStr (strexp, region) => map (regionify region) (check_strexp strexp)
      | VarStr _ => []

    and check_fctexp exp =
      case exp of
        AppFct _ => []
      | BaseFct {body, constraint, params} => check_strexp body
      | LetFct (dec, fctexp) => check dec @ (check_fctexp fctexp)
      | MarkFct (fctexp, region) => map (regionify region) (check_fctexp fctexp)
      | VarFct _ => []

    and check_fctb fctb =
      case fctb of
        Fctb {def, name} => check_fctexp def
      | MarkFctb (fc, region) => map (regionify region) (check_fctb fc)

    and check_strb strb =
      case strb of
        MarkStrb (strb, region) => map (regionify region) (check_strb strb)
      | Strb {constraint, def, name} => check_strexp def

    and check (d : dec) =
      case d of
        OpenDec _ => [(d, NONE)]
      | _ => []

    val warning = "using open at toplevel"
    val hint = "call functions with structure name rather than opening"
  end
