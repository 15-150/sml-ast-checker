structure NestedIfCase : CHECK =
  struct
    open Ast

    fun snd (x, y) = y

    fun rule (Rule {exp, pat = _}) = exp

    fun fun_dec (FunDec _) = true
      | fun_dec _ = false

    fun lambda (FnExp _) = true
      | lambda _ = false

    val find_exp_no_fun =
      fn x => AstTraverse.find_exp_with_cutoffs (fun_dec, lambda) x

    fun parts e =
      case e of
        CaseExp {expr, rules} =>
          map (fn e => (e, NONE)) (expr :: (map rule rules))
      | IfExp {test, thenCase, elseCase} =>
          [(test, NONE), (thenCase, NONE), (elseCase, NONE)]
      | _ => []

    fun too_nested (exp, _) =
      case find_exp_no_fun (Fn.const [], parts) exp of
        (_, []) => false
      | _ => true

    fun find_exactly_one exp =
      let val found = snd (find_exp_no_fun (Fn.const [], parts) exp) in
        (found, List.exists too_nested found)
      end

    fun nested (e, _) =
      case find_exp_no_fun (Fn.const [], parts) e of
        (_, []) => ([], false)
      | (_, L) =>
          let val found = map (fn (exp, _) => find_exactly_one exp) L in
            ( List.concat (map (fn (x, _) => x) found)
            , List.exists (fn (_, b) => b) found
            )
          end

    fun triple_cases e =
      let val found = map nested (parts e) in
        if List.exists (fn (_, b) => b) found then
          []
        else
          (case List.concat (map (fn (a, _) => a) found) of
             [] => []
           | _ => [(e, NONE)])
      end

    val check = triple_cases
    val warning = "three nestings of cases/ifs"
    val hint = "try using nested patterns or writing a helper function"
  end
