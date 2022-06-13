structure Functions = 
struct
  open Ast

  fun concatMap f L = List.concat (List.map f L)

  fun regionify region (path, NONE, fb) = (path, SOME region, fb)
    | regionify _ function = function

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
    | Fb (clauses, b) => concatMap (find_fns_from_clause fb) clauses

  and find_fns_from_clause (fb : fb) (Clause {exp, pats, resultty}) = 
    (find_fns_from_exp (SOME fb) exp)
    @ (fixitem_pat fb (List.hd pats)) (* First elem is fn name, rest are params *)

  and find_fns_from_pat fb pat =
    case pat of
      MarkPat (pat', region) => 
        List.map (regionify region) (find_fns_from_pat fb pat')
    | VarPat path => [(path, NONE, fb)]
    | _ => (print "TODO: Other cases of pats\n"; [])

    (* AppPat of {argument:pat, constr:pat}
  | CharPat of string
  | ConstraintPat of {constraint:ty, pattern:pat}
  | FlatAppPat of pat fixitem list
  | IntPat of literal
  | LayeredPat of {expPat:pat, varPat:pat}
  | ListPat of pat list
  | MarkPat of pat * region
  | OrPat of pat list
  | RecordPat of {def:(symbol * pat) list, flexibility:bool}
  | StringPat of string
  | TuplePat of pat list
  | VarPat of path
  | VectorPat of pat list
  | WildPat
  | WordPat of literal *)

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