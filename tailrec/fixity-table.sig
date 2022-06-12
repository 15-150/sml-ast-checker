signature FIXITY_TABLE =
  sig
    type t
    type table = t

    val basis : table
    val stringToTable : int -> bool -> string -> table
    val lookup : table -> Symbol.symbol -> Fixity.fixity option
    val insert : table -> (Symbol.symbol * Fixity.fixity) -> table
    val merge : table -> table -> table
    val trim : table -> table

    val fixityToVal : Fixity.fixity -> (int * bool) option
    val symOptToVal : table -> Symbol.symbol option -> (int * bool) option

    val maxFixitySymbols : table
                        -> (Symbol.symbol option * Ast.region) list
                        -> (int * bool) option * (Symbol.symbol option * Ast.region) list
    val findOuterSymbol : table
                       -> (Symbol.symbol option * Ast.region) list
                       -> (Symbol.symbol option * Ast.region) option
  end
