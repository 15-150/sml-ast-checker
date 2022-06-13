signature TAIL_REC =
  sig
    type var = Ast.path * Ast.region option
    type function = var * bool

    val find_exp : bool -> Ast.exp -> function list
    val find_dec : bool -> Ast.dec -> function list
    val find_fb : bool -> Ast.fb -> function list
  end
