structure Functions =
struct
  structure FT = FixityTable
  open Ast

  fun concatMap f L = List.concat (List.map f L)

  fun regionify region (path, NONE, fb, table) = (path, SOME region, fb, table)
    | regionify _ function = function
  fun mapfst f (l, t) = (List.map f l, t)

  fun getFunName table pats =
    let
      val tuplified = List.mapi (fn (i, {fixity, item, region}) => (fixity, i)) pats
    in
      case FT.findOuterSymbol table tuplified of
        NONE => List.hd pats (* No fixity information, the first thing must be function *)
      | SOME (_, t) => List.nth (pats, t) (* t = index of outer position *)
    end

  (* Returns a list of (path, region, fb, fixity_table), where fb is the
   * function body from the ast for the current function *)
  fun find_fns_from_exp (table : FT.t) (fb : fb option) exp =
    let
      val find_exp_no_table = find_fns_from_exp
      val find_fns_from_exp = find_fns_from_exp table
      val find_fns_from_rule = find_fns_from_rule table
    in
      case exp of
        AndalsoExp (e1, e2) =>
          (find_fns_from_exp fb e1) @ (find_fns_from_exp fb e2)
      | AppExp {argument:exp, function:exp} =>
          (find_fns_from_exp fb argument) @ (find_fns_from_exp fb function)
      | CaseExp {expr:exp, rules:rule list} =>
          (find_fns_from_exp fb expr) @ concatMap (find_fns_from_rule fb) rules
      | CharExp s => []
      | ConstraintExp {expr:exp, constraint:ty} => (find_fns_from_exp fb expr)
      | FlatAppExp [] => []
      | FlatAppExp exp_fixitems =>
          concatMap (fixitem_exp table fb) exp_fixitems
      | FnExp rules => concatMap (find_fns_from_rule fb) rules
      | HandleExp {expr:exp, rules:rule list} =>
          (find_fns_from_exp fb expr) @ concatMap (find_fns_from_rule fb) rules
      | IfExp {test : exp, thenCase : exp, elseCase : exp} =>
          (find_fns_from_exp fb test)
          @ (find_fns_from_exp fb thenCase) @ (find_fns_from_exp fb elseCase)
      | IntExp i => []
      | LetExp {dec:dec, expr:exp} =>
          let
            val (r, table') = find_fns_from_dec table fb dec
          in
            r @ (find_exp_no_table table' fb expr)
          end
      | ListExp exps =>
          concatMap (fn x => find_fns_from_exp fb x) exps
      | MarkExp (e, region) =>
          List.map (regionify region) (find_fns_from_exp fb e)
      | OrelseExp (e1, e2) => (find_fns_from_exp fb e1) @ (find_fns_from_exp fb e2)
      | RaiseExp e => (find_fns_from_exp fb e)
      | RealExp s => []
      | RecordExp fields =>
          concatMap (fn (_, exp) => find_fns_from_exp fb exp) fields
      | SelectorExp sym => []
      | SeqExp [] => []
      | SeqExp exps => concatMap (find_fns_from_exp fb) exps
      | StringExp s => []
      | TupleExp exps => concatMap (find_fns_from_exp fb) exps
      | VarExp path => [] (* [(path, NONE)] *)
      | VectorExp exps => concatMap (find_fns_from_exp fb) exps
      | WhileExp {test:exp, expr:exp} => raise Fail "todo maybe?"
      | WordExp i => []
    end

  (* returns ((path, region, fb, fixity_table) list, FixityTable.t) *)
  and find_fns_from_dec (table : FT.t) (fb : fb option) dec =
    case dec of
        AbstypeDec {abstycs : db list, body : dec, withtycs : tb list} => raise Fail "todo maybe?"
      | DataReplDec (sym, path) => ([], table)
      | DatatypeDec {datatycs : db list, withtycs : tb list} => ([], table)
      | DoDec exp => (find_fns_from_exp table fb exp, table) (* Successor ML extension *)
      | ExceptionDec ebs => ([], table)
      | FctDec fctb => ([], table) (* not legal *)
      | FixDec {fixity : fixity, ops : symbol list} =>
          ([], FT.insertAll table fixity ops)
      | FsigDec fsigs => ([], table)
      | FunDec (fbs, tys) => (concatMap (find_fns_from_fb table) fbs, table)
      | LocalDec (d1, d2) =>
          let
            val (r1, t1) = find_fns_from_dec table fb d1
            val (r2, _) = find_fns_from_dec t1 fb d2
            (* we need to re-run to find any changes in fixities just within
             * the `in ... end` portion of the declaration, so we use the old
             * table.
             *)
            val (_, t2) = find_fns_from_dec table fb d2
          in
            (r1 @ r2, t2)
          end
      | MarkDec (d', region) =>
          mapfst (regionify region) (find_fns_from_dec table fb d')
      | OpenDec paths => ([], table)
      | OvldDec x => raise Fail "idk what this is"
      | SeqDec decs =>
          List.foldl (fn (d, (l, t)) =>
            let
              val (l', t') = find_fns_from_dec t fb d
            in
              (l @ l', t')
            end
          ) ([], table) decs
      | SigDec sigs => ([], table)
      | StrDec strbs => ([], table)
      | TypeDec tb => ([], table)
      | ValDec (vbs, tys) => (concatMap (find_fns_from_vb table fb) vbs, table)
      | ValrecDec (rvbs, tys) => (concatMap (find_fns_from_rvb table fb) rvbs, table)

  and find_fns_from_rule table fb (Rule {exp, pat}) = find_fns_from_exp table fb exp

  and find_fns_from_fb table fb =
    case fb of
      MarkFb (fb', region) => List.map (regionify region) (find_fns_from_fb table fb')
      (* Every clause is from the same function, so we just need to get the function from the first clause *)
    | Fb (clauses, b) => find_fns_from_clause table fb (List.hd clauses)

  and find_fns_from_clause table (fb : fb) (Clause {exp, pats, resultty}) =
    let
      val name = getFunName table pats
    in
      (find_fns_from_exp table (SOME fb) exp) @ (fixitem_pat table fb name)
    end

  and find_fns_from_pat table fb pat =
    let
      val find_fns_from_pat = find_fns_from_pat table
    in
      case pat of
        AppPat {argument, constr} =>
          (find_fns_from_pat fb argument) @ (find_fns_from_pat fb constr)
      | CharPat s => []
      | ConstraintPat {constraint, pattern} => find_fns_from_pat fb pattern
      | FlatAppPat pats => concatMap (fixitem_pat table fb) pats
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
      | VarPat path => [(path, NONE, fb, table)]
      | VectorPat pats => concatMap (find_fns_from_pat fb) pats
      | WildPat => []
      | WordPat x => []
    end

  and find_fns_from_vb table fb vb =
    case vb of
      MarkVb (vb, region) => List.map (regionify region) (find_fns_from_vb table fb vb)
    | Vb {exp,lazyp,pat} => find_fns_from_exp table fb exp

  and find_fns_from_rvb table fb rvb =
    case rvb of
      MarkRvb (rvb, region) => List.map (regionify region) (find_fns_from_rvb table fb rvb)
    | Rvb {exp,fixity,lazyp,resultty, var} => find_fns_from_exp table fb exp

  and fixitem_exp table fb {fixity, item, region} =
    List.map (regionify region) (find_fns_from_exp table fb item)

  and fixitem_pat table fb {fixity, item, region} =
    List.map (regionify region) (find_fns_from_pat table fb item)

end
