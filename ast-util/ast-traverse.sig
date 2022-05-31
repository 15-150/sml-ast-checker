signature AST_TRAVERSE =
  sig
    (* An Ast.dec is returned by the parser, so it's easy to pass the parser output straight into
     * find_dec. However, other functions can be exposed as needed. *)
    val find_dec : (Ast.dec -> ('dec * (Ast.region option)) list)
                 * (Ast.exp -> ('exp * (Ast.region option)) list)
                 -> Ast.dec -> (('dec * (Ast.region option)) list)
                 * (('exp * (Ast.region option)) list)

    (* find_exp can be helpful for implementing new rules *)
    val find_exp : (Ast.dec -> ('dec * (Ast.region option)) list)
                 * (Ast.exp -> ('exp * (Ast.region option)) list)
                 -> Ast.exp -> (('dec * (Ast.region option)) list)
                 * (('exp * (Ast.region option)) list)

    (* These behave identically to find_dec and find_exp, but they allow you to specify two
     * predicates which, when true, stop the function from searching into that expression or dec *)
    val find_dec_with_cutoffs : (Ast.dec -> bool) * (Ast.exp -> bool)
                              -> (Ast.dec -> ('dec * (Ast.region option)) list)
                              * (Ast.exp -> ('exp * (Ast.region option)) list)
                              -> Ast.dec -> (('dec * (Ast.region option)) list)
                              * (('exp * (Ast.region option)) list)

    val find_exp_with_cutoffs : (Ast.dec -> bool) * (Ast.exp -> bool)
                              -> (Ast.dec -> ('dec * (Ast.region option)) list)
                              * (Ast.exp -> ('exp * (Ast.region option)) list)
                              -> Ast.exp -> (('dec * (Ast.region option)) list)
                              * (('exp * (Ast.region option)) list)
  end
