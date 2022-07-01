signature TAIL_REC =
  sig
    type var = Ast.path * Ast.region option
    type function = var * bool

    val find_exp : FixityTable.t -> bool -> Ast.exp -> function list
    val find_dec : FixityTable.t ->  bool -> Ast.dec -> function list * FixityTable.t
    val find_fb : FixityTable.t -> bool -> Ast.fb -> function list
    val find_vb : FixityTable.t -> bool -> Ast.vb -> function list
    val find_rvb : FixityTable.t -> bool -> Ast.rvb -> function list
  end
