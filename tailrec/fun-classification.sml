structure FunctionClassification =
struct

  datatype Function = Unknown of Ast.symbol list | NonRecursive | Recursive | TailRecursive

  (* Assumes the function is recursive, identifies Recursive or TailRecursive *)
  fun getRecursiveFunctionType function fnCalls classifications =
    let
      val recursiveCalls =
        List.filter (fn ((symbols, _), _) => function = symbols) fnCalls
      val numRecursiveCalls = List.length recursiveCalls
      val numTailRecursiveCalls =
        List.length (List.filter (fn ((_, _), tailRec) => tailRec) recursiveCalls)
      val fnType = 
        if numRecursiveCalls = numTailRecursiveCalls
        then TailRecursive else Recursive
    in
      (function, fnType) :: classifications
    end

  fun addFunctionType allFns function fb classifications =
    let
      val fnCalls = TailRec.find_fb true fb
      val callsSelf = List.exists (fn ((symbols, _), _) => function = symbols) fnCalls

      val nonSelfCalls = List.filter (fn ((f, _), _) => function <> f) fnCalls
      val fnsNonSelfCalls = 
        List.filter (fn (f1, _, _) => List.exists (fn ((f2, _), _) => f1 = f2) nonSelfCalls) allFns

      val calledFnClassifications = classifyFunctions [] fnsNonSelfCalls
      val recursiveCalls = List.exists (fn (f, t) => t = Recursive orelse t = TailRecursive) calledFnClassifications
      val allTailRecursiveCalls = List.all (fn (f, t) => t = TailRecursive orelse t = NonRecursive) calledFnClassifications
    in

      case (callsSelf, recursiveCalls, allTailRecursiveCalls) of
        (false, false, _) => (function, NonRecursive) :: (calledFnClassifications @ classifications)
      | (_, true, false) => (function, Recursive) :: (calledFnClassifications @ classifications)
      | (false, true, true) => (function, TailRecursive) :: (calledFnClassifications @ classifications)
      | (true, _, _) => getRecursiveFunctionType function fnCalls (calledFnClassifications @ classifications)
    end

  and getFunctionType allFns ((function, region, fb), classifications) =
    case List.find (fn (f, _) => function = f) classifications of
        SOME (_, t) => classifications (* Function has already been classified *)
      | NONE => addFunctionType allFns function fb classifications

  and classifyFunctions classifications fns =
    List.foldl (getFunctionType fns) classifications fns

  fun run fileName =
    let
      val source = Parse.getSource fileName
      val ast = SmlFile.parse source
      val fns = Functions.find_fns_from_dec NONE ast
    in
      classifyFunctions [] fns
    end
end
