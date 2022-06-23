signature TAIL_REC =
  sig
    type var = Ast.path * Ast.region option
    type function = var * bool

    val find_exp : FixityTable.t -> bool -> Ast.exp -> function list
    val find_dec : FixityTable.t ->  bool -> Ast.dec -> function list * FixityTable.t
    val find_fb : FixityTable.t -> bool -> Ast.fb -> function list
  end
