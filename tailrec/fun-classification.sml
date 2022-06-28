structure FunctionClassification =
struct
  structure FT = FixityTable

  datatype t
    = Unknown of Symbol.symbol list
    | NonRecursive
    | Recursive
    | TailRecursive
  type function = t

  type dec_info = Ast.region option * Ast.dec option

  local
    val to_path = List.rev o List.mapi (
        fn (0, sym) => Symbol.varSymbol sym
         | (_, sym) => Symbol.strSymbol sym) o List.rev
  in
  (* Basing these off of how we teach them, not their NJ implementation *)
    val init_classifications : ((Ast.path * (Ast.region option * Ast.dec option)) * function) list =
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

  fun listToString f l =
    let
      fun helper [] = ""
        | helper (x::[]) = f x
        | helper (x::xs) = (f x) ^ ", " ^ (helper xs)
    in
      "[" ^ (helper l) ^ "]\n"
    end

  (* Looks up the given function in the list of already known classifications,
   * as well as the initial list of classifications *)
  fun lookupFunctionType (classifications : (((Ast.path * dec_info) * function) list))
                         (function : Ast.path)
                       : (Ast.path * dec_info) * function =
    let
      val funAndType =
        List.find (fn ((f, _), _) => symbols_eq (function, f)) (classifications @ init_classifications)
    in
      case funAndType of
          SOME x => x
        | NONE => raise Fail ("Type not yet found " ^ (listToString Symbol.name function))
    end

  (* Assumes the function is recursive, identifies Recursive or TailRecursive *)
  fun getRecursiveFunctionType function dec_info fnCalls =
    let
      val recursiveCalls =
        List.filter (fn ((symbols, _), _) => symbols_eq (function, symbols)) fnCalls
      val numRecursiveCalls = List.length recursiveCalls
      val numTailRecursiveCalls =
        List.length (List.filter (fn ((_, _), tailRec) => tailRec) recursiveCalls)
      val fnType =
        if numRecursiveCalls = numTailRecursiveCalls
        then TailRecursive else Recursive
    in
      ((function, dec_info), fnType)
    end

  fun getFunctionType allFns ((function, dec_info, fb, table), classifications) =
    let
      val variables = TailRec.find_fb table true fb
      (* The tail recursion checker identifies all variables in the ast for the
       * fb, which includes both functions, and parameters. Now we filter to
       * only identify types of the functions in allFns, which includes all the
       * functions defined in the file, and the fns in in init_classifications *)
      val fnCalls = (List.map (fn ((f, r), t) => ((f, (r, NONE)), t)) o List.filter
          (fn ((f, r), t) => List.exists (fn (f', _) => symbols_eq (f, f')) allFns))
          variables

      val callsSelf = List.exists (fn ((f, _), _) => symbols_eq (function, f)) fnCalls

      val nonSelfCalls : ((Ast.path * dec_info) * bool) list =
        List.filter (fn ((f, _), _) => symbols_neq (function, f)) fnCalls
      val calledFnClassifications : ((Ast.path * dec_info) * function) list=
        List.map (lookupFunctionType classifications) (List.map (fn ((f, r), t) => f) nonSelfCalls)

      val recursiveCalls =
        List.exists (fn (_, t) => is_recursive t) calledFnClassifications
      val allTailRecursiveCalls =
        List.all (fn (_, t) => is_tailrecursive t orelse is_nonrecursive t) calledFnClassifications
    in
      (case (callsSelf, recursiveCalls, allTailRecursiveCalls) of
          (false, false, _) => ((function, dec_info), NonRecursive)
        | (_, true, false) => ((function, dec_info), Recursive)
        | (false, true, true) => ((function, dec_info), TailRecursive)
        | (true, _, _) => getRecursiveFunctionType function dec_info fnCalls
      ) :: classifications
    end

  fun classifyFunctions (allFns : (Ast.path * dec_info) list) (fns : (Ast.path * dec_info * Ast.fb * FT.table) list) =
    List.foldl (getFunctionType allFns) [] fns

  fun classifyAst dec =
    let
      val (fnsToClassify : (Ast.path * dec_info * Ast.fb * FT.t) list, _) =
        Functions.find_fns_from_dec FixityTable.basis NONE dec
      val allFns =
        (List.map (fn (p, r, _, _) => (p, r)) fnsToClassify)
        @ (List.map (fn (f, _) => f) init_classifications)
      val results = classifyFunctions allFns fnsToClassify
    in
      List.rev results
    end

  val simpleClassifyAst = List.map (fn ((p, _), c) => (p, c)) o classifyAst

end
