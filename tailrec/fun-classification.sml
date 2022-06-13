structure FunctionClassification =
struct

  datatype Function = Unknown of Ast.symbol list | NonRecursive | Recursive | TailRecursive

  (* Assumes the function is recursive, identifies Recursive or TailRecursive *)
  fun getRecursiveFunctionType function decs =
    let
      val recursiveCalls = 
        List.filter (fn ((symbols, _), _) => function = symbols) decs
      val numRecursiveCalls = List.length recursiveCalls
      val numTailRecursiveCalls = 
        List.length (List.filter (fn ((_, _), tailRec) => tailRec) recursiveCalls)
    in
      if numRecursiveCalls = numTailRecursiveCalls
      then TailRecursive else Recursive
    end

  fun getFunctionType ((function, region, fb) : Ast.path * Ast.region option * Ast.fb) =
    let
      val decs = TailRec.find_fb true fb
      val t = 
        if List.exists (fn ((symbols, _), _) => function = symbols) decs
        then getRecursiveFunctionType function decs
        else NonRecursive
    in
      (function, t)
    end

  fun classifyFunctions fileName =
    let
      val source = Parse.getSource fileName
      val ast = SmlFile.parse source
      val fns = Functions.find_fns_from_dec NONE ast
    in
      List.map getFunctionType fns
    end
end