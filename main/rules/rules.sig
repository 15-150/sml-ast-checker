local
  type 'a check = 'a * string * string * int
in
  signature RULES =
  sig
    (* To add more style rules, add structures in checker/ and then add them to the list here,
     * along with their associated weight *)
    val checkers        : (Ast.exp -> (Ast.exp * Ast.region option) list) check list

    (* These feed the entire top-level dec into the checker, rather than using the traverse function *)
    (* Use them to check top-level declaration style errors *)
    val dec_checkers    : (Ast.dec -> (Ast.dec * Ast.region option) list) check list

    (* These are checkers that are run on the text of the file instead of the AST *)
    val text_checkers   : (string -> bool) check list

    (* These are checkers that are run on both the text file and the AST. *)
    val hybrid_checkers : (string -> Ast.dec -> (Ast.exp * Ast.region option) list) check list
  end
end
