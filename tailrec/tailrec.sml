structure TailRec : TAIL_REC =
struct

  structure FT = FixityTable
  open Ast

  type var = path * region option
  type function = var * bool

  fun regionify region ((path, NONE), tail) = ((path, SOME region), tail)
    | regionify _ function = function

  fun find_exp tail exp =
    case exp of
        AndalsoExp (e1, e2) =>
          (find_exp false e1) @ (find_exp false e2)
      | AppExp {argument:exp, function:exp} =>
          (find_exp false argument) @ (find_exp tail function)
      | CaseExp {expr:exp, rules:rule list} =>
          (find_exp false expr) @ List.concat (List.map (find_rule tail) rules)
      | CharExp s => []
      | ConstraintExp {expr:exp, constraint:ty} => (find_exp tail expr)
      | FlatAppExp [] => []
      | FlatAppExp exp_fixitems =>
          let
            val tuplified = List.mapi (fn (i, {fixity, item, region}) => (fixity, i)) exp_fixitems
            val table = (print "TODO: update fixity table"; FT.basis)
          in
            case FT.findOuterSymbol table tuplified of
                NONE => (* No fixity information, the first thing must be tail *)
                  (fixitem tail (List.hd exp_fixitems))
                  @ List.concatMap (fixitem false) (List.tl exp_fixitems)
              | SOME (_, t) => (* t = index of tail position *)
                  List.concatMapi (fn (i, x) => fixitem (tail andalso i = t) x) exp_fixitems
          end
      | FnExp rules => List.concat (List.map (find_rule true) rules)
      | HandleExp {expr:exp, rules:rule list} =>
          (find_exp false expr) @ List.concat (List.map (find_rule tail) rules)
      | IfExp {test : exp, thenCase : exp, elseCase : exp} =>
          (find_exp false test) @ (find_exp tail thenCase) @ (find_exp tail elseCase)
      | IntExp i => []
      | LetExp {dec:dec, expr:exp} =>
          (find_dec false dec) @ (find_exp tail expr)
      | ListExp exps => List.concatMap (find_exp false) exps
      | MarkExp (e, region) =>
          List.map (regionify region) (find_exp tail e)
      | OrelseExp (e1, e2) =>
          (find_exp false e1) @ (find_exp false e2)
      | RaiseExp e => (find_exp false e)
      | RealExp s => []
      | RecordExp fields =>
          List.concatMap (fn (x, e) => find_exp false e) fields
      | SelectorExp sym => [(([sym], NONE), tail)]
      | SeqExp [] => []
      | SeqExp exps =>
          find_exp tail (List.last exps)
        @ List.concatMap (find_exp false) (List.take (exps, List.length exps - 1))
      | StringExp s => []
      | TupleExp exps =>
          List.concatMap (find_exp false) exps
      | VarExp path => [((path, NONE), tail)]
      | VectorExp exps =>
          List.concatMap (find_exp false) exps
      | WhileExp {test:exp, expr:exp} =>
          find_exp false test @ find_exp false expr
      | WordExp i => []

  and find_dec tail dec =
    case dec of
        AbstypeDec {abstycs : db list, body : dec, withtycs : tb list} => raise Fail "todo maybe?"
      | DataReplDec (sym, path) => []
      | DatatypeDec {datatycs : db list, withtycs : tb list} => []
      | DoDec exp => find_exp tail exp (* beats me! *)
      | ExceptionDec ebs => []
      | FctDec fctb => [] (* not legal *)
      | FixDec {fixity : fixity, ops : symbol list} => raise Fail "todo fixity"
      | FsigDec fsigs => []
      | FunDec (fbs, tys) => List.concatMap (find_fb tail) fbs
      | LocalDec (d1, d2) =>
          (find_dec false d1) @ (find_dec tail d2)
      | MarkDec (d', region) => List.map (regionify region) (find_dec tail d')
      | OpenDec paths => []
      | OvldDec x => raise Fail "idk what this is"
      | SeqDec decs =>
          find_dec tail (List.last decs)
          @ List.concatMap (find_dec false) (List.take (decs, List.length decs - 1))
      | SigDec sigs => []
      | StrDec strbs => []
      | TypeDec tb => []
      | ValDec (vbs, tys) => List.concatMap (find_vb tail) vbs
      | ValrecDec (rvbs, tys) => List.concatMap (find_rvb tail) rvbs
  and find_fb tail fb =
    case fb of
      MarkFb (fb, region) => List.map (regionify region) (find_fb tail fb)
    | Fb (clss, b) => List.concatMap (find_clause tail) clss
  and find_vb tail vb =
    case vb of
      MarkVb (vb, region) => List.map (regionify region) (find_vb tail vb)
    | Vb {exp,lazyp,pat} => find_exp tail exp
  and find_rvb tail rvb =
    case rvb of
      MarkRvb (rvb, region) => List.map (regionify region) (find_rvb tail rvb)
    | Rvb {exp,fixity,lazyp,resultty, var} => find_exp tail exp
  and find_clause tail (Clause {exp, pats, resultty}) = find_exp tail exp
  and find_rule tail (Rule {exp, pat}) = find_exp tail exp
  and fixitem tail {fixity, item, region} =
    List.map (regionify region) (find_exp tail item)

end
