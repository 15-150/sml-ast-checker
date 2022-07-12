signature TAIL_REC =
  sig
    type var = Ast.path * Ast.region option
    type call = var * bool * bool (* var, tail position, bound within *)

    val find_dec : FixityTable.t -> var list -> bool -> Ast.dec -> call list * FixityTable.t
    val find_exp : FixityTable.t -> var list -> bool -> Ast.exp -> call list
    val find_fb  : FixityTable.t -> var list -> bool -> Ast.fb  -> call list
    val find_vb  : FixityTable.t -> var list -> bool -> Ast.vb  -> call list
    val find_rvb : FixityTable.t -> var list -> bool -> Ast.rvb -> call list
  end
