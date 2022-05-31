structure RefUsage : CHECK =
  struct
    open Ast

    fun find_var e name =
      case e of
        MarkExp (e, r) => find_var e name
      | FlatAppExp [{fixity, item, region}] => find_var item name
      | VarExp [sym] => Symbol.name sym = name
      | _ => false

    fun find_ref e =
      case e of
        VarExp [sym] =>
          (let val name = Symbol.name sym in
             if name = "ref" orelse name = "!" orelse name = ":=" then
               [(e, NONE)]
             else
               []
           end)
      | _ => []

    val check = find_ref
    val warning = "ref, !, or := used"
    val hint = "You can do this functionally!"
  end
