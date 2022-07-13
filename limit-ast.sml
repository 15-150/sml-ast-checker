structure Ast =
struct

  open Ast

  datatype 'a sigConst = NoSig | Opaque of 'a | Transparent of 'a
  datatype exp
    = AndalsoExp of exp * exp
    | AppExp of {argument:exp, function:exp}
    | CaseExp of {expr:exp, rules:rule list}
    | CharExp of string
    | ConstraintExp of {constraint:ty, expr:exp}
    | FlatAppExp of exp fixitem list
    | FnExp of rule list
    (* | HandleExp of {expr:exp, rules:rule list} *)
    | IfExp of {elseCase:exp, test:exp, thenCase:exp}
    | IntExp of literal
    | LetExp of {dec:dec, expr:exp}
    | ListExp of exp list
    | MarkExp of exp * region
    | OrelseExp of exp * exp
    (* | RaiseExp of exp *)
    | RealExp of real_lit
    (* | RecordExp of (symbol * exp) list *)
    (* | SelectorExp of symbol *)
    (* | SeqExp of exp list *)
    | StringExp of string
    | TupleExp of exp list
    | VarExp of path
    (* | VectorExp of exp list *)
    (* | WhileExp of {expr:exp, test:exp} *)
    (* | WordExp of literal *)
  datatype rule = Rule of {exp:exp, pat:pat}
  datatype pat
    = AppPat of {argument:pat, constr:pat}
    | CharPat of string
    | ConstraintPat of {constraint:ty, pattern:pat}
    | FlatAppPat of pat fixitem list
    | IntPat of literal
    | LayeredPat of {expPat:pat, varPat:pat}
    | ListPat of pat list
    | MarkPat of pat * region
    (* do we want to exclude this? *)
    | OrPat of pat list
    (* | RecordPat of {def:(symbol * pat) list, flexibility:bool} *)
    | StringPat of string
    | TuplePat of pat list
    | VarPat of path
    (* | VectorPat of pat list *)
    | WildPat
    (* | WordPat of literal *)
  datatype strexp
    = AppStr of path * (strexp * bool) list
    | AppStrI of path * (strexp * bool) list
    | BaseStr of dec
    | ConstrainedStr of strexp * sigexp sigConst
    | LetStr of dec * strexp
    | MarkStr of strexp * region
    | VarStr of path
  datatype fctexp
    (* fsigexp sigConst must always be NoSig! *)
    = AppFct of path * (strexp * bool) list * fsigexp sigConst
    | BaseFct of {body:strexp, constraint:sigexp sigConst,
                  params:(symbol option * sigexp) list}
    | LetFct of dec * fctexp
    | MarkFct of fctexp * region
    | VarFct of path * fsigexp sigConst
  datatype wherespec
    = WhStruct of symbol list * symbol list
    | WhType of symbol list * tyvar list * ty
  datatype sigexp
    = AugSig of sigexp * wherespec list
    | BaseSig of spec list
    | MarkSig of sigexp * region
    | VarSig of symbol
  datatype fsigexp
    (* = BaseFsig of {param:(symbol option * sigexp) list, result:sigexp} *)
    = MarkFsig of fsigexp * region
    | VarFsig of symbol
  datatype spec
    = DataReplSpec of symbol * path
    | DataSpec of {datatycs:db list, withtycs:tb list}
    | ExceSpec of (symbol * ty option) list
    (* | FctSpec of (symbol * fsigexp) list *)
    | IncludeSpec of sigexp
    | MarkSpec of spec * region
    | ShareStrSpec of path list
    | ShareTycSpec of path list
    | StrSpec of (symbol * sigexp * path option) list
    | TycSpec of (symbol * tyvar list * ty option) list * bool
    | ValSpec of (symbol * ty) list
  datatype dec
    (* = AbstypeDec of {abstycs:db list, body:dec, withtycs:tb list} *)
    = DataReplDec of symbol * path
    | DatatypeDec of {datatycs:db list, withtycs:tb list}
    (* | DoDec of exp *) (* successor ML extension *)
    | ExceptionDec of eb list
    | FctDec of fctb list
    | FixDec of {fixity:fixity, ops:symbol list}
    (* | FsigDec of fsigb list *)
    | FunDec of fb list * tyvar list
    | LocalDec of dec * dec
    | MarkDec of dec * region
    | OpenDec of path list
    (* | OvldDec of symbol * exp list *)
    | SeqDec of dec list
    | SigDec of sigb list
    | StrDec of strb list
    | TypeDec of tb list
    | ValDec of vb list * tyvar list
    | ValrecDec of rvb list * tyvar list
  datatype vb = MarkVb of vb * region | Vb of {exp:exp, lazyp:bool, pat:pat}
  datatype rvb
    = MarkRvb of rvb * region
    | Rvb of {exp:exp, fixity:(symbol * region) option, lazyp:bool,
              resultty:ty option, var:symbol}
  datatype fb = Fb of clause list * bool | MarkFb of fb * region
  datatype clause
    = Clause of {exp:exp, pats:pat fixitem list, resultty:ty option}
  datatype tb
    = MarkTb of tb * region | Tb of {def:ty, tyc:symbol, tyvars:tyvar list}
  datatype db
    = Db of {lazyp:bool, rhs:(symbol * ty option) list, tyc:symbol,
             tyvars:tyvar list}
    | MarkDb of db * region
  datatype eb
    = EbDef of {edef:path, exn:symbol}
    | EbGen of {etype:ty option, exn:symbol}
    | MarkEb of eb * region
  datatype strb
    = MarkStrb of strb * region
    | Strb of {constraint:sigexp sigConst, def:strexp, name:symbol}
  datatype fctb
    = Fctb of {def:fctexp, name:symbol} | MarkFctb of fctb * region
  datatype sigb
    = MarkSigb of sigb * region | Sigb of {def:sigexp, name:symbol}
  (* datatype fsigb
    = Fsigb of {def:fsigexp, name:symbol} | MarkFsigb of fsigb * region *)
  datatype tyvar = MarkTyv of tyvar * region | Tyv of symbol
  datatype ty
    = ConTy of symbol list * ty list
    | MarkTy of ty * region
    (* | RecordTy of (symbol * ty) list *)
    | TupleTy of ty list
    | VarTy of tyvar

  (* Need to also remove top-level functions:
   *  print, op :=, op before, !, ignore, ref
   *  hd, tl, app
   *  valOf, isSome
   *)
end
