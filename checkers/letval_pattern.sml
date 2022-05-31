structure LetValPattern : MANUAL_CHECK =
  struct
    open Ast

    fun snd (x, y) = y

    fun is_banned name = case name of
        ("::" | "nil" | "[]") => true (* lists *)
      | ("SOME" | "NONE")     => true (* options *)
      | ("Empty" | "Node")    => true (* tree *)
      | _ => false

    fun check_pat p =
      case p of
        WildPat => false
      | VarPat [sym] => is_banned (Symbol.name sym)
      | VarPat _ => false
      | RecordPat {def, flexibility} => check_pats (map (fn (_, x) => x) def)
      | ConstraintPat {pattern, constraint} => check_pat pattern
      | LayeredPat {varPat, expPat} => check_pat varPat orelse check_pat expPat
      | VectorPat pats => check_pats pats
      | OrPat pats => check_pats pats
      | TuplePat pats => check_pats pats
      | MarkPat (pat, _) => check_pat pat
      | IntPat _ => true
      | CharPat _ => true
      | WordPat _ => true
      | StringPat _ => true
      | ListPat _ => true
      | FlatAppPat pats => check_pats (map (fn {item, ...} => item) pats)
      | AppPat _ => true

    and check_pats nil = false
      | check_pats (x :: xs) = check_pat x orelse check_pats xs

    fun check_val vb =
      case vb of
        Vb {pat, lazyp, exp} => check_pat pat
      | MarkVb (v, _) => check_val v


    fun find_val d =
      case d of
        ValDec (nil, _) => false
      | ValDec (x :: xs, _) => check_val x orelse find_val (ValDec (xs, nil))
      | SeqDec nil => false
      | SeqDec (x :: xs) => find_val x orelse find_val (SeqDec xs)
      | MarkDec (dec, _) => find_val dec
      | _ => false


    fun find_let e =
      case e of
        LetExp {dec, expr} => if find_val dec then [(e, NONE)] else nil
      | _ => []

    fun check _ dec = snd (AstTraverse.find_dec (Fn.const [], find_let) dec)
    val warning = "nonexhaustive val binding"
    val hint = "use a case expression instead"
  end
