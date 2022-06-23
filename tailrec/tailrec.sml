structure TailRec : TAIL_REC =
struct

  structure FT = FixityTable
  open Ast

  type var = path * region option
  type function = var * bool

  fun regionify region ((path, NONE), tail) = ((path, SOME region), tail)
    | regionify _ function = function
  fun mapfst f (l, t) = (List.map f l, t)

  fun find_exp table tail exp =
    case exp of
        AndalsoExp (e1, e2) =>
          (find_exp table false e1) @ (find_exp table false e2)
      | AppExp {argument:exp, function:exp} =>
          (find_exp table false argument) @ (find_exp table tail function)
      | CaseExp {expr:exp, rules:rule list} =>
          (find_exp table false expr) @ List.concatMap (find_rule table tail) rules
      | CharExp s => []
      | ConstraintExp {expr:exp, constraint:ty} => (find_exp table tail expr)
      | FlatAppExp [] => []
      | FlatAppExp exp_fixitems =>
          let
            val tuplified = List.mapi (fn (i, {fixity, item, region}) => (fixity, i)) exp_fixitems
          in
            case FT.findOuterSymbol table tuplified of
                NONE => (* No fixity information, the first thing must be tail *)
                  (fixitem table tail (List.hd exp_fixitems))
                  @ List.concatMap (fixitem table false) (List.tl exp_fixitems)
              | SOME (_, t) => (* t = index of tail position *)
                  List.concatMapi (fn (i, x) =>
                    fixitem table (tail andalso i = t) x
                  ) exp_fixitems
          end
      | FnExp rules => List.concatMap (find_rule table true) rules
      | HandleExp {expr:exp, rules:rule list} =>
          (find_exp table false expr) @ List.concatMap (find_rule table tail) rules
      | IfExp {test : exp, thenCase : exp, elseCase : exp} =>
          (find_exp table false test)
          @ (find_exp table tail thenCase)
          @ (find_exp table tail elseCase)
      | IntExp i => []
      | LetExp {dec:dec, expr:exp} =>
          let
            val (r, table') = find_dec table false dec
          in
            r @ (find_exp table' tail expr)
          end
      | ListExp exps => List.concatMap (find_exp table false) exps
      | MarkExp (e, region) =>
          List.map (regionify region) (find_exp table tail e)
      | OrelseExp (e1, e2) =>
          (find_exp table false e1) @ (find_exp table false e2)
      | RaiseExp e => (find_exp table false e)
      | RealExp s => []
      | RecordExp fields =>
          List.concatMap (fn (x, e) => find_exp table false e) fields
      | SelectorExp sym => [(([sym], NONE), tail)]
      | SeqExp [] => []
      | SeqExp exps =>
          find_exp table tail (List.last exps)
        @ List.concatMap (find_exp table false) (List.take (exps, List.length exps - 1))
      | StringExp s => []
      | TupleExp exps =>
          List.concatMap (find_exp table false) exps
      | VarExp path => [((path, NONE), tail)]
      | VectorExp exps =>
          List.concatMap (find_exp table false) exps
      | WhileExp {test:exp, expr:exp} =>
          find_exp table false test @ find_exp table false expr
      | WordExp i => []

  and find_dec table tail dec =
    case dec of
        AbstypeDec {abstycs : db list, body : dec, withtycs : tb list} => raise Fail "todo maybe?"
      | DataReplDec (sym, path) => ([], table)
      | DatatypeDec {datatycs : db list, withtycs : tb list} => ([], table)
      | DoDec exp => (find_exp table tail exp, table) (* Successor ML extension *)
      | ExceptionDec ebs => ([], table)
      | FctDec fctb => ([], table) (* not legal *)
      | FixDec {fixity : fixity, ops : symbol list} =>
          ([], FT.insertAll table fixity ops)
      | FsigDec fsigs => ([], table)
      | FunDec (fbs, tys) => (List.concatMap (find_fb table tail) fbs, table)
      | LocalDec (d1, d2) =>
          let
            val (r1, t1) = find_dec table false d1
            val (r2, _) = find_dec t1 tail d2
            (* we need to re-run to find any changes in fixities just within
             * the `in ... end` portion of the declaration, so we use the old
             * table.
             *)
            val (_, t2) = find_dec table tail d2
          in
            (r1 @ r2, t2)
          end
      | MarkDec (d', region) => mapfst (regionify region) (find_dec table tail d')
      | OpenDec paths => ([], table)
      | OvldDec x => raise Fail "idk what this is"
      | SeqDec decs =>
          let
            val (l, t) = List.foldl (fn (d, (l, t)) =>
              let
                val (l', t') = find_dec t false d
              in
                (l @ l', t')
              end
            ) ([], table) (List.take (decs, List.length decs - 1))
            val (l1, t) = find_dec t tail (List.last decs)
          in
            (l @ l1, t)
          end
      | SigDec sigs => ([], table)
      | StrDec strbs => ([], table)
      | TypeDec tb => ([], table)
      | ValDec (vbs, tys) => (List.concatMap (find_vb table tail) vbs, table)
      | ValrecDec (rvbs, tys) => (List.concatMap (find_rvb table tail) rvbs, table)
  and find_fb table tail fb =
    case fb of
      MarkFb (fb, region) => List.map (regionify region) (find_fb table tail fb)
    | Fb (clss, b) => List.concatMap (find_clause table tail) clss
  and find_vb table tail vb =
    case vb of
      MarkVb (vb, region) => List.map (regionify region) (find_vb table tail vb)
    | Vb {exp,lazyp,pat} => find_exp table tail exp
  and find_rvb table tail rvb =
    case rvb of
      MarkRvb (rvb, region) => List.map (regionify region) (find_rvb table tail rvb)
    | Rvb {exp,fixity,lazyp,resultty, var} => find_exp table tail exp
  and find_clause table tail (Clause {exp, pats, resultty}) = find_exp table tail exp
  and find_rule table tail (Rule {exp, pat}) = find_exp table tail exp
  and fixitem table tail {fixity, item, region} =
    List.map (regionify region) (find_exp table tail item)

end
