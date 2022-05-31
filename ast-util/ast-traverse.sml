structure AstTraverse : AST_TRAVERSE =
  struct

    open Ast

    fun regionify region (elem, NONE) = (elem, SOME region)
      | regionify _ (elem, SOME region) = (elem, SOME region)

    infix @@
    fun (a, b) @@ (x, y) = (a @ x, b @ y)

    val mapexp = fn f => fn (d, e) => (d, List.map f e)
    val mapdec = fn f => fn (d, e) => (List.map f d, e)
    val concat = fn l => List.foldl op @@ ([], []) l
    val concatMap = fn f => List.foldMapl op @@ f ([], [])

    (* This is not the find_exp exposed in the signature. That's at the bottom of this file. *)
    fun find_exp (pd, pe) (f as (fd, fe)) e =
      if pe e then
        ([], [])
      else
        case e of
          AndalsoExp (e1, e2) =>
            ([], fe e1)
            @@ ([], fe e2)
            @@ find_exp (pd, pe) f e1
            @@ find_exp (pd, pe) f e2
        | AppExp {argument : exp, function : exp} =>
            ([], fe argument)
            @@ ([], fe function)
            @@ find_exp (pd, pe) f argument
            @@ find_exp (pd, pe) f function
        | CaseExp {expr : exp, rules : rule list} =>
            ([], fe expr)
            @@ find_exp (pd, pe) (fd, fe) expr
            @@ concat (map (find_rule (pd, pe) f) rules)
        | CharExp str => ([], [])
        | ConstraintExp {constraint : ty, expr : exp} =>
            ([], fe expr) @@ find_exp (pd, pe) f expr
        | FlatAppExp exp_fixitems => concatMap (fixitem (pd, pe) f) exp_fixitems
        | FnExp rules => concat (map (find_rule (pd, pe) f) rules)
        | HandleExp {expr : exp, rules : rule list} =>
            ([], fe expr)
            @@ find_exp (pd, pe) f expr
            @@ concat (map (find_rule (pd, pe) f) rules)
        | IfExp {elseCase : exp, test : exp, thenCase : exp} =>
            ([], fe elseCase)
            @@ ([], fe test)
            @@ ([], fe thenCase)
            @@ find_exp (pd, pe) f elseCase
            @@ find_exp (pd, pe) f test
            @@ find_exp (pd, pe) f thenCase
        | IntExp i => ([], [])
        | LetExp {dec : dec, expr : exp} =>
            (fd dec, fe expr)
            @@ find_dec (pd, pe) f dec
            @@ find_exp (pd, pe) f expr
        | ListExp exps =>
            ([], List.concatMap fe exps) @@ concatMap (find_exp (pd, pe) f) exps
        | MarkExp (e', region) =>
            mapexp (regionify region) (([], fe e') @@ find_exp (pd, pe) f e')
        | OrelseExp (e1, e2) =>
            ([], fe e1)
            @@ ([], fe e2)
            @@ find_exp (pd, pe) f e1
            @@ find_exp (pd, pe) f e2
        | RaiseExp e' => ([], fe e') @@ find_exp (pd, pe) f e'
        | RealExp str => ([], [])
        | RecordExp fields =>
            concatMap (fn (_, e') => ([], fe e') @@ find_exp (pd, pe) f e') fields
        | SelectorExp symbol => ([], [])
        | SeqExp exps =>
            concatMap (find_exp (pd, pe) f) exps @@ ([], List.concat (map fe exps))
        | StringExp strs => ([], [])
        | TupleExp exps =>
            concatMap (find_exp (pd, pe) f) exps @@ ([], List.concat (map fe exps))
        | VarExp path => ([], [])
        | VectorExp exps =>
            concatMap (find_exp (pd, pe) f) exps @@ ([], List.concat (map fe exps))
        | WhileExp {expr : exp, test : exp} =>
            ([], fe expr)
            @@ ([], fe test)
            @@ find_exp (pd, pe) f expr
            @@ find_exp (pd, pe) f test
        | WordExp i => ([], [])


    and fixitem (pd, pe) (f as (fd, fe)) {fixity, item, region} =
      mapexp (regionify region) (([], fe item) @@ find_exp (pd, pe) f item)

    and find_rule (pd, pe) f (Rule {exp, pat}) = find_exp (pd, pe) f exp

    and find_dec (pd, pe) (f as (fd, fe)) d =
      if pd d then
        ([], [])
      else
        (case d of
           AbstypeDec {abstycs : db list, body : dec, withtycs : tb list} =>
             ([], [])
         | DataReplDec (sym, path) => ([], [])
         | DatatypeDec {datatycs : db list, withtycs : tb list} => ([], [])
         | DoDec exp => find_exp (pd, pe) f exp
         | ExceptionDec ebs => ([], [])
         | FctDec fctb => concatMap (find_fctb (pd, pe) f) fctb
         | FixDec {fixity : fixity, ops : symbol list} => ([], [])
         | FsigDec fsigs => ([], [])
         | FunDec (fbs, tys) => concatMap (find_fbs (pd, pe) f) fbs
         | LocalDec (d1, d2) =>
             (fd d1, [])
             @@ (fd d2, [])
             @@ find_dec (pd, pe) f d1
             @@ find_dec (pd, pe) f d2
         | MarkDec (d', region) =>
             mapdec (regionify region) ((fd d', []) @@ find_dec (pd, pe) f d')
         | OpenDec paths => ([], [])
         | OvldDec x =>
             (let val (sym, exps) = Dec.ovld x in
                concatMap (find_exp (pd, pe) f) exps @@ ([], List.concatMap fe exps)
              end)
         | SeqDec decs =>
             concatMap (find_dec (pd, pe) f) decs @@ (List.concat (map fd decs), [])
         | SigDec sigs => ([], [])
         | StrDec strbs => concatMap (find_strb (pd, pe) f) strbs
         | TypeDec tb => ([], [])
         | ValDec (vbs, tys) => concatMap (find_vb (pd, pe) f) vbs
         | ValrecDec (rvbs, tys) => concatMap (find_rvb (pd, pe) f) rvbs)
          handle Match =>
            (Dec.legacy {abs = (fn strbs => concatMap (find_strb (pd, pe) f) strbs)} d)


    and find_strb (pd, pe) f strb =
      case strb of
        MarkStrb (strb, region) =>
          mapexp (regionify region) (find_strb (pd, pe) f strb)
      | Strb {constraint, def, name} => find_strexp (pd, pe) f def

    and find_strexp (pd, pe) (f as (fd, fe)) strexp =
      case strexp of
        AppStr (longid, strexps) =>
          concatMap (fn (s, b) => find_strexp (pd, pe) f s) strexps
      | AppStrI (longid, strexps) =>
          concatMap (fn (s, b) => find_strexp (pd, pe) f s) strexps
      | BaseStr dec => (fd dec, []) @@ find_dec (pd, pe) f dec
      | ConstrainedStr (strexp, sigconst) => find_strexp (pd, pe) f strexp
      | LetStr (dec, strexp) =>
          (fd dec, [])
          @@ find_dec (pd, pe) f dec
          @@ find_strexp (pd, pe) f strexp
      | MarkStr (strexp, region) =>
          mapexp (regionify region) (find_strexp (pd, pe) f strexp)
      | VarStr path => ([], [])

    and find_fctb (pd, pe) f fctb =
      case fctb of
        MarkFctb (fct, region) =>
          mapexp (regionify region) (find_fctb (pd, pe) f fct)
      | Fctb {def, name} => find_fctexp (pd, pe) f def

    and find_fctexp (pd, pe) (f as (fd, fe)) fct =
      case fct of
        AppFct (path, strexp, sigexp) =>
          concatMap (fn (s, b) => find_strexp (pd, pe) f s) strexp
      | BaseFct {body, constraint, params} => find_strexp (pd, pe) f body
      | LetFct (dec, fctexp) =>
          (fd dec, [])
          @@ find_dec (pd, pe) f dec
          @@ find_fctexp (pd, pe) f fctexp
      | MarkFct (fctexp, region) =>
          mapexp (regionify region) (find_fctexp (pd, pe) f fctexp)
      | VarFct (path, fsig) => ([], [])

    and find_fbs (pd, pe) f fb =
      case fb of
        MarkFb (fb, region) =>
          mapexp (regionify region) (find_fbs (pd, pe) f fb)
      | Fb (clss, b) => concatMap (find_clause (pd, pe) f) clss

    and find_clause (pd, pe) (f as (fd, fe)) (Clause {exp, pats, resultty}) =
      ([], fe exp) @@ find_exp (pd, pe) f exp

    and find_vb (pd, pe) (f as (fd, fe)) vb =
      case vb of
        MarkVb (vb', region) =>
          mapexp (regionify region) (find_vb (pd, pe) f vb')
      | Vb {exp, lazyp, pat} => ([], fe exp) @@ find_exp (pd, pe) f exp

    and find_rvb (pd, pe) (f as (fd, fe)) rvb =
      case rvb of
        MarkRvb (rvb', region) =>
          mapexp (regionify region) (find_rvb (pd, pe) f rvb')
      | Rvb {exp, fixity, lazyp, resultty, var} =>
          ([], fe exp) @@ find_exp (pd, pe) f exp

    fun find_exp_with_cutoffs (pd, pe) (f as (fd, fe)) e =
      if pe e then
        ([], [])
      else
        ([], fe e) @@ find_exp (pd, pe) f e

    fun find_dec_with_cutoffs (pd, pe) (f as (fd, fe)) d =
      if pd d then
        ([], [])
      else
        (fd d, []) @@ find_dec (pd, pe) f d

    (* Gives find_exp a more intuitive behavior for the outside user *)
    val find_exp =
      fn (fd, fe) =>
        fn e => ([], fe e) @@ find_exp (fn _ => false, fn _ => false) (fd, fe) e

    val find_dec =
      fn (fd, fe) =>
        fn d => (fd d, []) @@ find_dec (fn _ => false, fn _ => false) (fd, fe) d

  end
