structure TailRec : TAIL_REC =
struct
  open Ast

  type var = path * region option
  type function = var * bool

  fun regionify region ((path, NONE), tail) = ((path, SOME region), tail)
    | regionify _ function = function

  fun find_exp exp tail =
    case exp of
        AndalsoExp (e1, e2) =>
          (find_exp e1 false) @ (find_exp e2 false)
      | AppExp {argument:exp, function:exp} =>
          (find_exp argument false) @ (find_exp function tail)
      | CaseExp {expr:exp, rules:rule list} =>
          (find_exp expr false) @ List.concat (List.map (find_rule tail) rules)
      | CharExp s => []
      | ConstraintExp {expr:exp, constraint:ty} => (find_exp expr tail)
      (* help *)
      | FlatAppExp [] => []
      | FlatAppExp (exp_fixitem::exp_fixitems) => (print "TODO: fixity!!!!";
          (fixitem tail exp_fixitem) @ List.concat (List.map (fixitem false) exp_fixitems))
      | FnExp rules => List.concat (List.map (find_rule true) rules)
      | HandleExp {expr:exp, rules:rule list} =>
          (find_exp expr false) @ List.concat (List.map (find_rule tail) rules)
      | IfExp {test : exp, thenCase : exp, elseCase : exp} =>
          (find_exp test false) @ (find_exp thenCase tail) @ (find_exp elseCase tail)
      | IntExp i => []
      | LetExp {dec:dec, expr:exp} =>
          (find_dec dec false) @ (find_exp expr tail)
      | ListExp exps =>
          List.concat (List.map (fn x => find_exp x false) exps)
      | MarkExp (e, region) =>
          List.map (regionify region) (find_exp e tail)
      | OrelseExp (e1, e2) =>
          (find_exp e1 false) @ (find_exp e2 false)
      | RaiseExp e => (find_exp e false)
      | RealExp s => []
      | RecordExp fields =>
          List.concat (List.map (fn (x, e) => find_exp e false) fields)
      | SelectorExp sym => [(([sym], NONE), tail)]
      | SeqExp [] => []
      | SeqExp exps =>
          find_exp (List.last exps) tail
        @ List.concat (List.map (fn e => find_exp e false) (List.take (exps, List.length exps - 1)))
      | StringExp s => []
      | TupleExp exps =>
          List.concat (List.map (fn x => find_exp x false) exps)
      | VarExp path => [((path, NONE), tail)]
      | VectorExp exps =>
          List.concat (List.map (fn x => find_exp x false) exps)
      | WhileExp {test:exp, expr:exp} =>
          find_exp test false @ find_exp expr false
      | WordExp i => []

  and find_dec dec tail =
    case dec of
        AbstypeDec {abstycs : db list, body : dec, withtycs : tb list} => raise Fail "todo maybe?"
      | DataReplDec (sym, path) => []
      | DatatypeDec {datatycs : db list, withtycs : tb list} => []
      | DoDec exp => find_exp exp tail (* beats me! *)
      | ExceptionDec ebs => []
      | FctDec fctb => [] (* not legal *)
      | FixDec {fixity : fixity, ops : symbol list} => raise Fail "todo fixity"
      | FsigDec fsigs => []
      | FunDec (fbs, tys) => List.concat (List.map (find_fb tail) fbs)
      | LocalDec (d1, d2) =>
          (find_dec d1 false) @ (find_dec d2 tail)
      | MarkDec (d', region) => List.map (regionify region) (find_dec d' tail)
      | OpenDec paths => []
      | OvldDec x => raise Fail "idk what this is"
      | SeqDec decs =>
          find_dec (List.last decs) tail
          @ List.concat (List.map (fn d => find_dec d false) (List.take (decs, List.length decs - 1)))
      | SigDec sigs => []
      | StrDec strbs => []
      | TypeDec tb => []
      | ValDec (vbs, tys) => List.concat (List.map (find_vb tail) vbs)
      | ValrecDec (rvbs, tys) => List.concat (List.map (find_rvb tail) rvbs)
  and find_fb tail fb =
    case fb of
      MarkFb (fb, region) => List.map (regionify region) (find_fb tail fb)
    | Fb (clss, b) => List.concat (List.map (find_clause tail) clss)
  and find_vb tail vb =
    case vb of
      MarkVb (vb, region) => List.map (regionify region) (find_vb tail vb)
    | Vb {exp,lazyp,pat} => find_exp exp tail
  and find_rvb tail rvb =
    case rvb of
      MarkRvb (rvb, region) => List.map (regionify region) (find_rvb tail rvb)
    | Rvb {exp,fixity,lazyp,resultty, var} => find_exp exp tail
  and find_clause tail (Clause {exp, pats, resultty}) = find_exp exp tail
  and find_rule tail (Rule {exp, pat}) = find_exp exp tail
  and fixitem tail {fixity, item, region} =
    List.map (regionify region) (find_exp item tail)

end
