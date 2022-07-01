structure Functions =
struct
  structure FT = FixityTable
  open Ast

  fun regionify region (path, (NONE, dec), fb, table) = (path, (SOME region, dec), fb, table)
    | regionify _ function = function
  fun mapfst f (l, t) = (List.map f l, t)

  datatype output = OutFb of fb | OutVb of vb | OutRvb of rvb

  fun getFunName table pats =
    let
      val tuplified = List.mapi (fn (i, {fixity, item, region}) => (fixity, i)) pats
    in
      case FT.findOuterSymbol table tuplified of
        NONE => List.hd pats (* No fixity information, the first thing must be function *)
      | SOME (_, t) => List.nth (pats, t) (* t = index of outer position *)
    end

  (* Returns a list of (path, region, output, fixity_table), where output is the
   * function/val/valrec body from the ast for the current function *)
  fun find_fns_from_exp (table : FT.t) (out : output option) exp =
    let
      val find_exp_no_table = find_fns_from_exp
      val find_fns_from_exp = find_fns_from_exp table
      val find_fns_from_rule = find_fns_from_rule table
    in
      case exp of
        AndalsoExp (e1, e2) =>
          (find_fns_from_exp out e1)
          @ (find_fns_from_exp out e2)
      | AppExp {argument:exp, function:exp} =>
          (find_fns_from_exp out argument)
          @ (find_fns_from_exp out function)
      | CaseExp {expr:exp, rules:rule list} =>
          (find_fns_from_exp out expr)
          @ List.concatMap (find_fns_from_rule out) rules
      | CharExp s => []
      | ConstraintExp {expr:exp, constraint:ty} => (find_fns_from_exp out expr)
      | FlatAppExp [] => []
      | FlatAppExp exp_fixitems =>
          List.concatMap (fixitem_exp table out) exp_fixitems
      | FnExp rules => List.concatMap (find_fns_from_rule out) rules
      | HandleExp {expr:exp, rules:rule list} =>
          (find_fns_from_exp out expr)
          @ List.concatMap (find_fns_from_rule out) rules
      | IfExp {test : exp, thenCase : exp, elseCase : exp} =>
          (find_fns_from_exp out test)
          @ (find_fns_from_exp out thenCase)
          @ (find_fns_from_exp out elseCase)
      | IntExp i => []
      | LetExp {dec:dec, expr:exp} =>
          let
            val (r, table') = find_fns_from_dec table out dec
          in
            r @ (find_exp_no_table table' out expr)
          end
      | ListExp exps =>
          List.concatMap (fn x => find_fns_from_exp out x) exps
      | MarkExp (e, region) =>
          List.map (regionify region) (find_fns_from_exp out e)
      | OrelseExp (e1, e2) =>
          (find_fns_from_exp out e1)
          @ (find_fns_from_exp out e2)
      | RaiseExp e => (find_fns_from_exp out e)
      | RealExp s => []
      | RecordExp fields =>
          List.concatMap (fn (_, exp) => find_fns_from_exp out exp) fields
      | SelectorExp sym => []
      | SeqExp [] => []
      | SeqExp exps => List.concatMap (find_fns_from_exp out) exps
      | StringExp s => []
      | TupleExp exps => List.concatMap (find_fns_from_exp out) exps
      | VarExp path => [] (* [(path, NONE)] *)
      | VectorExp exps => List.concatMap (find_fns_from_exp out) exps
      | WhileExp {test:exp, expr:exp} => raise Fail "todo maybe?"
      | WordExp i => []
    end

  (* returns ((path, region, out, fixity_table) list, FixityTable.t) *)
  and find_fns_from_dec (table : FT.t) (out : output option) dec =
    case dec of
        AbstypeDec {abstycs : db list, body : dec, withtycs : tb list} => ([], table)
      | DataReplDec (sym, path) => ([], table)
      | DatatypeDec {datatycs : db list, withtycs : tb list} => ([], table)
      | DoDec exp => (find_fns_from_exp table out exp, table) (* Successor ML extension *)
      | ExceptionDec ebs => ([], table)
      | FctDec fctb => ([], table) (* not legal *)
      | FixDec {fixity : fixity, ops : symbol list} =>
          ([], FT.insertAll table fixity ops)
      | FsigDec fsigs => ([], table)
      | FunDec (fbs, tys) =>
          (List.concatMap (find_fns_from_fb table (SOME dec)) fbs, table)
      | LocalDec (d1, d2) =>
          let
            val (r1, t1) = find_fns_from_dec table out d1
            val (r2, _) = find_fns_from_dec t1 out d2
            (* we need to re-run to find any changes in fixities just within
             * the `in ... end` portion of the declaration, so we use the old
             * table.
             *)
            val (_, t2) = find_fns_from_dec table out d2
          in
            (r1 @ r2, t2)
          end
      | MarkDec (d', region) =>
          mapfst (regionify region) (find_fns_from_dec table out d')
      | OpenDec paths => ([], table)
      | OvldDec x => raise Fail "idk what this is"
      | SeqDec decs =>
          List.foldl (fn (d, (l, t)) =>
            let
              val (l', t') = find_fns_from_dec t out d
            in
              (l @ l', t')
            end
          ) ([], table) decs
      | SigDec sigs => ([], table)
      | StrDec strbs => ([], table)
      | TypeDec tb => ([], table)
      | ValDec (vbs, tys) =>
          (List.concatMap (find_fns_from_vb table (SOME dec)) vbs, table)
      | ValrecDec (rvbs, tys) =>
          (List.concatMap (find_fns_from_rvb table (SOME dec)) rvbs, table)

  and find_fns_from_rule table out (Rule {exp, pat}) = find_fns_from_exp table out exp

  and find_fns_from_fb table dec fb =
    case fb of
      MarkFb (fb', region) => List.map (regionify region) (find_fns_from_fb table dec fb')
      (* Every clause is from the same function, so we just need to get the function from the first clause *)
    | Fb (clauses, b) => find_fns_from_clause table dec (OutFb fb) (List.hd clauses)

  and find_fns_from_clause table dec (out : output) (Clause {exp, pats, resultty}) =
    let
      val name = getFunName table pats
    in
      (find_fns_from_exp table (SOME out) exp) @ (fixitem_pat table dec out name)
    end

  and find_fns_from_pat table dec out pat =
    let
      val find_fns_from_pat = find_fns_from_pat table dec
    in
      case pat of
        AppPat {argument, constr} =>
          (find_fns_from_pat out argument)
          @ (find_fns_from_pat out constr)
      | CharPat s => []
      | ConstraintPat {constraint, pattern} => find_fns_from_pat out pattern
      | FlatAppPat pats => List.concatMap (fixitem_pat table dec out) pats
      | IntPat x => []
      | LayeredPat {expPat, varPat} =>
          (find_fns_from_pat out expPat)
          @ (find_fns_from_pat out varPat)
      | ListPat pats => List.concatMap (find_fns_from_pat out) pats
      | MarkPat (pat', region) =>
          List.map (regionify region) (find_fns_from_pat out pat')
      | OrPat pats => List.concatMap (find_fns_from_pat out) pats
      | RecordPat {def, flexibility} =>
          List.concatMap (fn (symbol, pat) => find_fns_from_pat out pat) def
      | StringPat s => []
      | TuplePat pats => List.concatMap (find_fns_from_pat out) pats
      | VarPat path => [(path, (NONE, dec), out, table)]
      | VectorPat pats => List.concatMap (find_fns_from_pat out) pats
      | WildPat => []
      | WordPat x => []
    end

  and find_fns_from_vb table dec vb =
    case vb of
      MarkVb (vb, region) =>
        List.map (regionify region) (find_fns_from_vb table dec vb)
    | Vb {exp,lazyp,pat} =>
        find_fns_from_pat table dec (OutVb vb) pat
        @ find_fns_from_exp table (SOME (OutVb vb)) exp

  and find_fns_from_rvb table dec rvb =
    case rvb of
      MarkRvb (rvb, region) =>
        List.map (regionify region) (find_fns_from_rvb table dec rvb)
    | Rvb {exp,fixity,lazyp,resultty, var} =>
        ([var], (NONE, dec), OutRvb rvb, table)
        :: find_fns_from_exp table (SOME (OutRvb rvb)) exp

  and fixitem_exp table out {fixity, item, region} =
    List.map (regionify region) (find_fns_from_exp table out item)

  and fixitem_pat table dec out {fixity, item, region} =
    List.map (regionify region) (find_fns_from_pat table dec out item)

end
