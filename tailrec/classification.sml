structure Classification : CLASSIFICATION =
struct
  datatype classification
    = Unknown of Symbol.symbol list
    | NonRecursive
    | Recursive
    | TailRecursive
  type t = classification

  type dec_info = Ast.region option * Ast.dec option
  type classifications = ((Ast.path * dec_info) * classification) list

  val toString =
   fn Unknown syms =>
        "[" ^ String.concatWith ", " (List.map Symbol.name syms) ^ "]"
    | NonRecursive => "NonRecursive"
    | Recursive => "Recursive"
    | TailRecursive => "TailRecursive"

  fun classificationsToString cls =
    String.concatWith "\n" (
      List.map (fn ((path, _), c) =>
        String.concatWith "." (List.map Symbol.name path)
        ^ " is " ^ toString c) cls
    )

  local
    val to_path = List.rev o List.mapi (
        fn (0, sym) => Symbol.varSymbol sym
         | (_, sym) => Symbol.strSymbol sym) o List.rev
  in
  (* Basing these off of how we teach them, not their NJ implementation *)
    val init_classifications : classifications =
      List.map (fn (syms, cls) => ((to_path syms, (NONE, NONE)), cls))
        [ (["length"], Recursive)
        , (["List", "length"], Recursive)
        , (["@"], Recursive)
        , (["rev"], Recursive)
        , (["List", "rev"], Recursive)
        , (["List", "tabulate"], Recursive)
        , (["map"], Recursive)
        , (["List", "map"], Recursive)
        , (["List", "mapPartial"], Recursive)
        , (["List", "filter"], Recursive)
        , (["foldl"], TailRecursive)
        , (["List", "foldl"], TailRecursive)
        , (["foldr"], Recursive)
        , (["List", "foldr"], Recursive)
        , (["List", "zip"], Recursive)
        , (["+"], NonRecursive)
        , (["-"], NonRecursive)
        , (["*"], NonRecursive)
        , (["/"], NonRecursive)
        , (["div"], NonRecursive)
        , (["mod"], NonRecursive)
        , (["^"], NonRecursive)
        , (["o"], NonRecursive)
        , ([":="], NonRecursive)
        , (["before"], NonRecursive)
        , (["ignore"], NonRecursive)
        , (["@"], Recursive)
        , (["::"], NonRecursive)
        , (["="], NonRecursive)
        , (["<"], NonRecursive)
        , ([">"], NonRecursive)
        , (["<="], NonRecursive)
        , ([">="], NonRecursive)
        ]
  end

  val symbols_eq = ListPair.allEq Symbol.eq
  val symbols_neq = not o symbols_eq

  val eq =
    fn (Unknown s, Unknown s') => symbols_eq (s, s')
     | (NonRecursive, NonRecursive) => true
     | (Recursive, Recursive) => true
     | (TailRecursive, TailRecursive) => true
     | (_,_) => false
  val is_tailrecursive = fn TailRecursive => true | _ => false
  val is_recursive = fn Recursive => true | f => is_tailrecursive f
  val is_nonrecursive = fn NonRecursive => true | _ => false
end
