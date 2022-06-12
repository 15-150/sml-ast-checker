signature TAIL_REC =
  sig
    type var = Ast.path * Ast.region option
    type function = var * bool

    val find_exp : Ast.exp -> bool -> function list
    val find_dec : Ast.dec -> bool -> function list
  end
