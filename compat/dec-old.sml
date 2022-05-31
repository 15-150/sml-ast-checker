structure Dec =
  struct

    val ovld = fn (sym,_,exps) => (sym,exps)

    val legacy = fn {abs} => fn
      Ast.AbsDec strbs => abs strbs
    | _ => raise Fail "Invalid case"

  end
