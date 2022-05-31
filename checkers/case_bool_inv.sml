structure CaseBoolInv : CHECK =
  struct
    open Ast

    datatype case_pats = NotFound | Wildcard | Var of string

    fun find_var_exp e name =
      case e of
        MarkExp (e, r) => find_var_exp e name
      | FlatAppExp [{fixity, item, region}] => find_var_exp item name
      | VarExp [sym] => Symbol.name sym = name
      | _ => false

    fun find_var_pat p =
      case p of
        MarkPat (p, r) => find_var_pat p
      | FlatAppPat [{fixity, item, region}] => find_var_pat item
      | VarPat [sym] => Var (Symbol.name sym)
      | WildPat => Wildcard
      | _ => NotFound

    fun find_clause1 e p =
      case find_var_pat p of
        NotFound => NONE
      | Wildcard => NONE
      | Var "true" => if find_var_exp e "false" then SOME true else NONE
      | Var "false" => if find_var_exp e "true" then SOME false else NONE
      | Var _ => NONE

    val find_var_inv = fn e => find_var_exp e o Bool.toString

    fun find_clause2 e p clause1 =
      case find_var_pat p of
        NotFound => false
      | Wildcard => find_var_inv e clause1
      | Var _ => find_var_inv e clause1

    fun find_clauses (Rule {exp = e1, pat = p1}) (Rule {exp = e2, pat = p2}) =
      case find_clause1 e1 p1 of
        NONE => false
      | SOME clause1 => find_clause2 e2 p2 clause1

    fun find_case e =
      case e of
        CaseExp {expr = _, rules = [r1, r2]} =>
          (if find_clauses r1 r2 then
             [(e, NONE)]
           else
             [])
      | _ => []

    val check = find_case
    val warning =
      "(case <bool> of true => false | false => true) or some variation"
    val hint = "you don't need the case, just use not!"
  end
