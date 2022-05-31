signature MANUAL_CHECK =
  sig
    val check : string -> Ast.dec -> ((Ast.exp * Ast.region option) list)
    val warning : string
    val hint : string
  end
