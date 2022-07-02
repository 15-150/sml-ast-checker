signature TAIL_REC =
  sig
    type var = Ast.path * Ast.region option
    type call = var * bool

    val find_dec : FixityTable.t -> bool -> Ast.dec -> call list * FixityTable.t
    val find_exp : FixityTable.t -> bool -> Ast.exp -> call list
    val find_fb  : FixityTable.t -> bool -> Ast.fb  -> call list
    val find_vb  : FixityTable.t -> bool -> Ast.vb  -> call list
    val find_rvb : FixityTable.t -> bool -> Ast.rvb -> call list
  end
