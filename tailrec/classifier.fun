functor MkClassifier (
  include CLASSIFICATION
) = struct

  structure FT = FixityTable

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
  fun lookupFunctionType (classifications : (((Ast.path * dec_info) * classification) list))
                         (function : Ast.path)
                       : (Ast.path * dec_info) * classification =
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

  fun getTailRec table =
    fn Functions.OutFb fb   => TailRec.find_fb table true fb
     | Functions.OutVb vb   => TailRec.find_vb table true vb
     | Functions.OutRvb rvb => TailRec.find_rvb table true rvb

  fun getFunctionType allFns ((function, dec_info, out, table), classifications) =
    let
      val variables = getTailRec table out
      (* The tail recursion checker identifies all variables in the ast for the
       * out, which includes both functions, and parameters. Now we filter to
       * only identify types of the functions in allFns, which includes all the
       * functions defined in the file, and the fns in in init_classifications *)
      val fnCalls = (List.map (fn ((f, r), t) => ((f, (r, NONE)), t)) o List.filter
          (fn ((f, r), t) => List.exists (fn (f', _) => symbols_eq (f, f')) allFns))
          variables

      val callsSelf = List.exists (fn ((f, _), _) => symbols_eq (function, f)) fnCalls

      val nonSelfCalls : ((Ast.path * dec_info) * bool) list =
        List.filter (fn ((f, _), _) => symbols_neq (function, f)) fnCalls
      val calledFnClassifications : ((Ast.path * dec_info) * classification) list=
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

  fun classifyFunctions (allFns : (Ast.path * dec_info) list)
                        (fns : (Ast.path * dec_info * Functions.output * FT.table) list) =
    List.foldl (getFunctionType allFns) [] fns

  fun classifyAst dec =
    let
      val (fnsToClassify : (Ast.path * dec_info * Functions.output * FT.t) list, _) =
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
