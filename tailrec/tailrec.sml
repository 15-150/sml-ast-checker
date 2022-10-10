structure TailRec : TAIL_REC =
struct

  structure FT = FixityTable
  open Ast

  type var = path * region option
  type call = var * bool * bool

  fun regionify region ((path, NONE), tail, bound) = ((path, SOME region), tail, bound)
    | regionify _ call = call
  fun mapfst f (l, t) = (List.map f l, t)

  fun regionify_bound region (path, NONE) = (path, SOME region)
    | regionify_bound _ call = call

  fun get_bound_vars_from_pat pat =
    case pat of
      AppPat {argument, constr} => 
        (get_bound_vars_from_pat argument) @ (get_bound_vars_from_pat constr)
    | CharPat s => []
    | ConstraintPat {constraint, pattern} => get_bound_vars_from_pat pattern
    | FlatAppPat pats => List.concatMap fixitem_pat pats
    | IntPat x => []
    | LayeredPat {expPat, varPat} => 
        (get_bound_vars_from_pat expPat) @ (get_bound_vars_from_pat varPat)
    | ListPat pats => List.concatMap get_bound_vars_from_pat pats
    | MarkPat (pat', region) => 
        List.map (regionify_bound region) (get_bound_vars_from_pat pat')
    | OrPat pats => List.concatMap get_bound_vars_from_pat pats
    | RecordPat {def, flexibility} => 
        List.concatMap (fn (symbol, pat) => get_bound_vars_from_pat pat) def
    | StringPat s => []
    | TuplePat pats => List.concatMap get_bound_vars_from_pat pats
    | VarPat path => [(path, NONE)]
    | VectorPat pats => List.concatMap get_bound_vars_from_pat pats
    | WildPat => []
    | WordPat x => []
  and fixitem_pat {fixity, item, region} =
    List.map (regionify_bound region) (get_bound_vars_from_pat item)
  fun get_bound_vars_from_rule (Rule {exp, pat}) =
    get_bound_vars_from_pat pat

  fun find_exp table bound tail exp =
    case exp of
        AndalsoExp (e1, e2) =>
          (find_exp table bound false e1)
          @ (find_exp table bound false e2)
      | AppExp {argument:exp, function:exp} =>
          (find_exp table bound false argument)
          @ (find_exp table bound tail function)
      | CaseExp {expr:exp, rules:rule list} =>
          (find_exp table bound false expr)
          @ List.concatMap (find_rule table bound tail) rules
      | CharExp s => []
      | ConstraintExp {expr:exp, constraint:ty} => (find_exp table bound tail expr)
      | FlatAppExp [] => []
      | FlatAppExp exp_fixitems =>
          let
            val tuplified = List.mapi (fn (i, {fixity, item, region}) => (fixity, i)) exp_fixitems
          in
            case FT.findOuterSymbol table tuplified of
                NONE => (* No fixity information, the first thing must be tail *)
                  (fixitem_exp table bound tail (List.hd exp_fixitems))
                  @ List.concatMap (fixitem_exp table bound false) (List.tl exp_fixitems)
              | SOME (_, t) => (* t = index of tail position *)
                  List.concatMapi (fn (i, x) =>
                    fixitem_exp table bound (tail andalso i = t) x
                  ) exp_fixitems
          end
      | FnExp rules => List.concatMap (find_rule table bound true) rules
      | HandleExp {expr:exp, rules:rule list} =>
          (find_exp table bound false expr)
          @ List.concatMap (find_rule table bound tail) rules
      | IfExp {test : exp, thenCase : exp, elseCase : exp} =>
          (find_exp table bound false test)
          @ (find_exp table bound tail thenCase)
          @ (find_exp table bound tail elseCase)
      | IntExp i => []
      | LetExp {dec:dec, expr:exp} =>
          let
            val (_, table') = find_dec table bound false dec
          in
            find_exp table' bound tail expr
          end
      | ListExp exps => List.concatMap (find_exp table bound false) exps
      | MarkExp (e, region) =>
          List.map (regionify region) (find_exp table bound tail e)
      | OrelseExp (e1, e2) =>
          (find_exp table bound false e1)
          @ (find_exp table bound false e2)
      | RaiseExp e => (find_exp table bound false e)
      | RealExp s => []
      | RecordExp fields =>
          List.concatMap (fn (x, e) => find_exp table bound false e) fields
      | SelectorExp sym => (* Not really sure about this one *)
          if List.exists (fn (boundSym, region) => boundSym = [sym]) bound
          then [(([sym], NONE), tail, true)]
          else [(([sym], NONE), tail, false)]
      | SeqExp [] => []
      | SeqExp exps =>
          find_exp table bound tail (List.last exps)
          @ List.concatMap (find_exp table bound false) (List.take (exps, List.length exps - 1))
      | StringExp s => []
      | TupleExp exps =>
          List.concatMap (find_exp table bound false) exps
      | VarExp path => 
          if List.exists (fn (boundPath, region) => boundPath = path) bound
          then [((path, NONE), tail, true)]
          else [((path, NONE), tail, false)]
      | VectorExp exps =>
          List.concatMap (find_exp table bound false) exps
      | WhileExp {test:exp, expr:exp} =>
          find_exp table bound false test
          @ find_exp table bound false expr
      | WordExp i => []

  and find_dec table bound tail dec =
    case dec of
        AbstypeDec {abstycs : db list, body : dec, withtycs : tb list} =>
          raise Fail "todo maybe?"
      | DataReplDec (sym, path) => ([], table)
      | DatatypeDec {datatycs : db list, withtycs : tb list} => ([], table)
      | DoDec exp => (find_exp table bound tail exp, table) (* Successor ML extension *)
      | ExceptionDec ebs => ([], table)
      | FctDec fctb => ([], table) (* not legal *)
      | FixDec {fixity : fixity, ops : symbol list} =>
          ([], FT.insertAll table fixity ops)
      | FsigDec fsigs => ([], table)
      | FunDec (fbs, tys) => (List.concatMap (find_fb table bound tail) fbs, table)
      | LocalDec (d1, d2) =>
          let
            val (r1, t1) = find_dec table bound false d1
            val (r2, _) = find_dec t1 bound tail d2
            (* we need to re-run to find any changes in fixities just within
             * the `in ... end` portion of the declaration, so we use the old
             * table.
             *)
            val (_, t2) = find_dec table bound tail d2
          in
            (r1 @ r2, t2)
          end
      | MarkDec (d', region) =>
          mapfst (regionify region) (find_dec table bound tail d')
      | OpenDec paths => ([], table)
      | OvldDec x => raise Fail "idk what this is"
      | SeqDec decs =>(
          case decs of
            [] => ([], table)
          | _  =>
            let
              val (l, t) = List.foldl (fn (d, (l, t)) =>
                let
                  val (l', t') = find_dec t bound false d
                in
                  (l @ l', t')
                end
              ) ([], table) (List.take (decs, List.length decs - 1))
              val (l1, t) = find_dec t bound tail (List.last decs)
            in
              (l @ l1, t)
            end)
      | SigDec sigs => ([], table)
      | StrDec strbs => ([], table)
      | TypeDec tb => ([], table)
      | ValDec (vbs, tys) =>
          (List.concatMap (find_vb table bound tail) vbs, table)
      | ValrecDec (rvbs, tys) =>
          (List.concatMap (find_rvb table bound tail) rvbs, table)
  and find_fb table bound tail fb =
    case fb of
      MarkFb (fb, region) => List.map (regionify region) (find_fb table bound tail fb)
    | Fb (clss, b) => List.concatMap (find_clause table bound tail) clss
  and find_vb table bound tail vb =
    case vb of
      MarkVb (vb, region) => 
        List.map (regionify region) (find_vb table bound tail vb)
    | Vb {exp,lazyp,pat} => 
        find_exp table (bound @ (get_bound_vars_from_pat pat)) tail exp
  and find_rvb table bound tail rvb =
    case rvb of
      MarkRvb (rvb, region) =>
        List.map (regionify region) (find_rvb table bound tail rvb)
    | Rvb {exp,fixity,lazyp,resultty, var} => find_exp table bound tail exp
  and find_clause table bound tail (Clause {exp, pats, resultty}) =
    (* We drop the first pattern since that's the function name *)
    find_exp table (bound @ (List.concatMap fixitem_pat (List.drop (pats, 1)))) tail exp
  and find_rule table bound tail (Rule {exp, pat}) =
    find_exp table (bound @ (get_bound_vars_from_rule (Rule {exp=exp, pat=pat}))) tail exp
  and fixitem_exp table bound tail {fixity, item, region} =
    List.map (regionify region) (find_exp table bound tail item)
end
