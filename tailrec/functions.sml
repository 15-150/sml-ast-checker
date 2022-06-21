structure Functions =
struct
  structure FT = FixityTable
  open Ast

  fun concatMap f L = List.concat (List.map f L)

  fun regionify region (path, NONE, fb) = (path, SOME region, fb)
    | regionify _ function = function

  (* Returns a list of (path, region, fb), where fb is the function body
   * from the ast for the current function *)
  fun find_fns_from_exp (fb : fb option) exp =
    case exp of
        AndalsoExp (e1, e2) =>
          (find_fns_from_exp fb e1) @ (find_fns_from_exp fb e2)
      | AppExp {argument:exp, function:exp} =>
          (find_fns_from_exp fb argument) @ (find_fns_from_exp fb function)
      | CaseExp {expr:exp, rules:rule list} =>
          (find_fns_from_exp fb expr) @ concatMap (find_fns_from_rule fb) rules
      | CharExp s => []
      | ConstraintExp {expr:exp, constraint:ty} => (find_fns_from_exp fb expr)
      (* help *)
      | FlatAppExp [] => []
      | FlatAppExp exp_fixitems =>
          concatMap (fixitem_exp fb) exp_fixitems
      | FnExp rules => concatMap (find_fns_from_rule fb) rules
      | HandleExp {expr:exp, rules:rule list} =>
          (find_fns_from_exp fb expr) @ concatMap (find_fns_from_rule fb) rules
      | IfExp {test : exp, thenCase : exp, elseCase : exp} =>
          (find_fns_from_exp fb test)
          @ (find_fns_from_exp fb thenCase) @ (find_fns_from_exp fb elseCase)
      | IntExp i => []
      | LetExp {dec:dec, expr:exp} =>
          (find_fns_from_dec fb dec) @ (find_fns_from_exp fb expr)
      | ListExp exps =>
          concatMap (fn x => find_fns_from_exp fb x) exps
      | MarkExp (e, region) =>
          List.map (regionify region) (find_fns_from_exp fb e)
      | OrelseExp (e1, e2) => (find_fns_from_exp fb e1) @ (find_fns_from_exp fb e2)
      | RaiseExp e => (find_fns_from_exp fb e)
      | RealExp s => []
      | RecordExp fields =>
          concatMap (fn (_, exp) => find_fns_from_exp fb exp) fields
      | SelectorExp sym => raise Fail "todo maybe?"
      | SeqExp [] => []
      | SeqExp exps => concatMap (find_fns_from_exp fb) exps
      | StringExp s => []
      | TupleExp exps => concatMap (find_fns_from_exp fb) exps
      | VarExp path => [] (* [(path, NONE)] *)
      | VectorExp exps => concatMap (find_fns_from_exp fb) exps
      | WhileExp {test:exp, expr:exp} => raise Fail "todo maybe?"
      | WordExp i => []

  and find_fns_from_dec (fb : fb option) dec =
    case dec of
        AbstypeDec {abstycs : db list, body : dec, withtycs : tb list} => raise Fail "todo maybe?"
      | DataReplDec (sym, path) => []
      | DatatypeDec {datatycs : db list, withtycs : tb list} => []
      | DoDec exp => find_fns_from_exp fb exp (* beats me! *)
      | ExceptionDec ebs => []
      | FctDec fctb => [] (* not legal *)
      | FixDec {fixity : fixity, ops : symbol list} => raise Fail "todo fixity"
      | FsigDec fsigs => []
      | FunDec (fbs, tys) => (concatMap (find_fns_from_fb) fbs)
      | LocalDec (d1, d2) =>
          (find_fns_from_dec fb d1) @ (find_fns_from_dec fb d2)
      | MarkDec (d', region) => List.map (regionify region) (find_fns_from_dec fb d')
      | OpenDec paths => []
      | OvldDec x => raise Fail "idk what this is"
      | SeqDec decs => concatMap (find_fns_from_dec fb) decs
      | SigDec sigs => []
      | StrDec strbs => []
      | TypeDec tb => []
      | ValDec (vbs, tys) => concatMap (find_fns_from_vb fb) vbs
      | ValrecDec (rvbs, tys) => concatMap (find_fns_from_rvb fb) rvbs

  and find_fns_from_rule fb (Rule {exp, pat}) = find_fns_from_exp fb exp

  and find_fns_from_fb fb =
    case fb of
      MarkFb (fb', region) => List.map (regionify region) (find_fns_from_fb fb')
      (* Every clause is from the same function, so we just need to get the function from the first clause *)
    | Fb (clauses, b) => find_fns_from_clause fb (List.hd clauses)

  and find_fns_from_clause (fb : fb) (Clause {exp, pats, resultty}) =
    let
      val tuplified = List.mapi (fn (i, {fixity, item, region}) => (fixity, i)) pats
      val table = FT.basis (* TODO: update this table! *)
      val name =
        case FT.findOuterSymbol table tuplified of
            NONE => List.hd pats (* No fixity information, the first thing must be function *)
          | SOME (_, t) => List.nth (pats, t) (* t = index of outer position *)
    in
      (find_fns_from_exp (SOME fb) exp) @ (fixitem_pat fb name)
    end

  and find_fns_from_pat fb pat =
    case pat of
      AppPat {argument, constr} =>
        (find_fns_from_pat fb argument) @ (find_fns_from_pat fb constr)
    | CharPat s => []
    | ConstraintPat {constraint, pattern} => find_fns_from_pat fb pattern
    | FlatAppPat pats => concatMap (fixitem_pat fb) pats
    | IntPat x => []
    | LayeredPat {expPat, varPat} =>
        (find_fns_from_pat fb expPat) @ (find_fns_from_pat fb varPat)
    | ListPat pats => concatMap (find_fns_from_pat fb) pats
    | MarkPat (pat', region) =>
        List.map (regionify region) (find_fns_from_pat fb pat')
    | OrPat pats => concatMap (find_fns_from_pat fb) pats
    | RecordPat {def, flexibility} =>
        concatMap (fn (symbol, pat) => find_fns_from_pat fb pat) def
    | StringPat s => []
    | TuplePat pats => concatMap (find_fns_from_pat fb) pats
    | VarPat path => [(path, NONE, fb)]
    | VectorPat pats => concatMap (find_fns_from_pat fb) pats
    | WildPat => []
    | WordPat x => []

  and find_fns_from_vb fb vb =
    case vb of
      MarkVb (vb, region) => List.map (regionify region) (find_fns_from_vb fb vb)
    | Vb {exp,lazyp,pat} => find_fns_from_exp fb exp

  and find_fns_from_rvb fb rvb =
    case rvb of
      MarkRvb (rvb, region) => List.map (regionify region) (find_fns_from_rvb fb rvb)
    | Rvb {exp,fixity,lazyp,resultty, var} => find_fns_from_exp fb exp

  and fixitem_exp fb {fixity, item, region} =
    List.map (regionify region) (find_fns_from_exp fb item)

  and fixitem_pat fb {fixity, item, region} =
    List.map (regionify region) (find_fns_from_pat fb item)

end
