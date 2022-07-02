signature CLASSIFICATION =
sig
  datatype classification
    = Unknown of Symbol.symbol list
    | NonRecursive
    | Recursive
    | TailRecursive
  type t = classification

  type dec_info = Ast.region option * Ast.dec option
  type classifications = ((Ast.path * dec_info) * classification) list

  val init_classifications : classifications

  val symbols_eq : Symbol.symbol list * Symbol.symbol list -> bool
  val symbols_neq : Symbol.symbol list * Symbol.symbol list -> bool

  val eq : classification * classification -> bool
  val is_tailrecursive : classification -> bool
  val is_recursive : classification -> bool
  val is_nonrecursive : classification -> bool
end
