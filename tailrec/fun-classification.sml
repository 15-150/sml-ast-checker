structure FunctionClassification =
struct

  datatype t
    = Unknown of Symbol.symbol list
    | NonRecursive
    | Recursive
    | TailRecursive
  type function = t

  local
    val to_path = List.rev o List.mapi (
        fn (0, sym) => Symbol.varSymbol sym
         | (_, sym) => Symbol.strSymbol sym) o List.rev
  in
  (* Basing these off of how we teach them, not their NJ implementation *)
    val init_classifications = List.map (fn (syms, cls) => (to_path syms, cls))
      [ (["length"], Recursive)
      , (["@"], Recursive)
      , (["rev"], Recursive)
      , (["List", "tabulate"], Recursive)
      , (["map"], Recursive)
      , (["List", "mapPartial"], Recursive)
      , (["List", "filter"], Recursive)
      , (["foldl"], TailRecursive)
      , (["foldr"], Recursive)
      , (["List", "zip"], Recursive)
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

  (* Assumes the function is recursive, identifies Recursive or TailRecursive *)
  fun getRecursiveFunctionType function fnCalls =
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
      (function, fnType)
    end

  fun addFunctionType allFns function fb classifications =
    let
      val fnCalls = TailRec.find_fb true fb
      val callsSelf = List.exists (fn ((symbols, _), _) => symbols_eq (function, symbols)) fnCalls

      val nonSelfCalls = List.filter (fn ((f, _), _) => symbols_neq (function, f)) fnCalls
      val fnsNonSelfCalls =
        List.filter (fn (f1, _, _) => List.exists (fn ((f2, _), _) => symbols_eq (f1,f2)) nonSelfCalls) allFns

      val calledFnClassifications = classifyFunctions [] fnsNonSelfCalls
      val recursiveCalls = 
        List.exists (fn (f, t) => t = Recursive orelse t = TailRecursive) calledFnClassifications
      val allTailRecursiveCalls = 
        List.all (fn (f, t) => t = TailRecursive orelse t = NonRecursive) calledFnClassifications
    in
      (case (callsSelf, recursiveCalls, allTailRecursiveCalls) of
          (false, false, _) => (function, NonRecursive)
        | (_, true, false) => (function, Recursive)
        | (false, true, true) => (function, TailRecursive)
        | (true, _, _) => getRecursiveFunctionType function fnCalls
      ) :: (calledFnClassifications @ classifications)
    end

  and getFunctionType allFns ((function, region, fb), classifications) =
    case List.find (fn (f, _) => symbols_eq (function, f)) classifications of
        SOME (_, t) => classifications (* Function has already been classified *)
      | NONE => addFunctionType allFns function fb classifications

  and classifyFunctions classifications fns =
    List.foldl (getFunctionType fns) classifications fns

  fun run fileName =
    let
      val source = Parse.getSource fileName
      val ast = SmlFile.parse source
      val fns = Functions.find_fns_from_dec NONE ast
      val results = classifyFunctions init_classifications fns
    in
      List.filter (fn (f1,t1) =>
        not (List.exists (fn (f2, t2) => symbols_eq (f1, f2) andalso eq (t1, t2)) init_classifications)
      ) results
    end
end
