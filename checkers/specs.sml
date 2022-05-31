structure Specs =
  struct
    open Ast

    infix ^^
    fun (s ^^ (x :: xs)) = (s ^ x) :: xs
      | (s ^^ []) = [s]

    infix ::::
    fun (s :::: (x :: xs)) = (s :: x) :: xs
      | (s :::: []) = [[s]]


    fun comments _ [] = []
      | comments _ [_] = []
      | comments n (x :: y :: xs) =
        case (x, y) of
          (#"(", #"*") => "(*" ^^ comments (n + 1) xs
        | (#"*", #")") =>
            if n = 1 then
              "*)" :: (comments 0 xs)
            else if n > 0 then
              "*)" ^^ (comments (n - 1) xs)
            else
              comments 0 xs
        | (_, #"(") =>
            if n > 0 then
              (String.implode [x]) ^^ (comments n (y :: xs))
            else
              comments n (y :: xs)
        | (_, #"*") =>
            if n > 0 then
              (String.implode [x]) ^^ (comments n (y :: xs))
            else
              comments n (y :: xs)
        | (_, _) =>
            if n > 0 then
              (String.implode [x, y]) ^^ (comments n xs)
            else
              comments n xs

    fun find_tail p [] = []
      | find_tail p (x :: xs) = if p x then xs else find_tail p xs


    (* The built in fields function removes delimiters, but I want to keep them *)
    fun fields p s =
      let
        fun fields' [] = []
          | fields' (c :: cs) =
            if p c then
              [] :: ([c] :: (fields' cs))
            else
              c :::: fields' cs
      in
        map String.implode (fields' (String.explode s))
      end

    fun separate s =
      fields
        (fn (#" ") => true
          | (#"(") => true
          | (#")") => true
          | (#"\n") => true
          | (#":") => true
          | _ => false)
        s

    fun match name =
      not
      o List.null
      o (find_tail (String.isSubstring "->"))
      o (find_tail (String.isSubstring ":"))
      o (find_tail (fn x => name = x))
      o separate

    fun match_spec comments name =
      String.concat (List.filter (match name) comments)

    fun get_pat_name pat =
      case pat of
        FlatAppPat [p] => get_pat_name (# item p)
      | MarkPat (p, region) => get_pat_name p
      | VarPat path => (VarExp path, NONE)
      | _ => raise Fail "Syntax Error"

    fun get_fun_name fb =
      case fb of
        Fb ((Clause {exp, pats, resultty}) :: cls, _) =>
          get_pat_name (# item (List.hd pats))
      | MarkFb (fb, region) =>
          (let val (v, _) = get_fun_name fb in (v, SOME region) end)
      | _ => raise Fail "Syntax Error"

    fun get_valrec_name rvb =
      case rvb of
        Rvb {exp, fixity, lazyp, resultty, var} => (VarExp [var], NONE)
      | MarkRvb (rvb, region) =>
          (let val (v, _) = get_valrec_name rvb in (v, SOME region) end)

    fun get_names_str strb =
      case strb of
        MarkStrb (strb, region) => get_names_str strb
      | Strb {constraint, def, name} =>
          case def of
            MarkStr (str, region) =>
              get_names_str
                (Strb {constraint = constraint, def = str, name = name})
          | BaseStr dec => find_rec_fun dec
          | _ => []

    and get_names_fct fctb =
      case fctb of
        MarkFctb (fctb, region) => get_names_fct fctb
      | Fctb {def, name} =>
          case def of
            MarkFct (fct, region) =>
              get_names_fct (Fctb {def = fct, name = name})
          | BaseFct {body, constraint, params} =>
              get_names_str (Strb {constraint = NoSig, def = body, name = name})
          | _ => []


    and find_rec_fun dec =
      case dec of
        FunDec (fbs, _) => map get_fun_name fbs
      | ValrecDec (rvbs, _) => map get_valrec_name rvbs
      | LocalDec (d1, d2) => find_rec_fun d1 @ find_rec_fun d2
      | MarkDec (d, region) => find_rec_fun d
      | SeqDec decs => List.concatMap find_rec_fun decs
      | StrDec strbs => List.concatMap get_names_str strbs
      | FctDec fctbs => List.concatMap get_names_fct fctbs
      | _ => []

    fun find_funs exp =
      case exp of
        LetExp {dec, expr} => find_rec_fun dec
      | _ => []

    fun extract_name exp =
      case exp of
        VarExp [symbol] => Symbol.name symbol
      | _ => raise Fail "Invalid function name"

    fun check_specs starter_specs text ast =
      let
        val comments = starter_specs @ comments 0 (String.explode text)
        val recursive_functions =
          find_rec_fun ast @ AstTraverse.find_dec find_funs ast
      in
        List.filter
          (fn (ast, region_opt) => match_spec comments (extract_name ast) = "")
          recursive_functions
      end

    (* This needs to deal with functions that don't have specs in the starter code -
     * maybe we could manually input them? *)
    fun check starter_code_functions =
      let
        val starter_specs =
          map (fn s => s ^ " : 'a -> 'b") starter_code_functions
      in
        check_specs starter_specs
      end

    val warning = "Missing specs."
    val hint = "all non-trivial functions need specs."

  end
