structure FixityTable : FIXITY_TABLE =
struct

  infix |>
  fun x |> f = f x
  fun fst (x,y) = x
  fun snd (x,y) = y

  type t = (Symbol.symbol * Fixity.fixity) list
  type table = t

  fun stringToTable fixity left sym =
    ( Symbol.fixSymbol sym
    , (if left then Fixity.infixleft else Fixity.infixright) fixity
    )::[]
  fun stringlistToTable (symlist, fixity, right) =
    List.concatMap (stringToTable fixity right) symlist

  val basis : table = List.concatMap stringlistToTable
    [ (["*", "/", "div", "mod"], 7, true)
    , (["+", "-", "^"], 6, true)
    , (["::", "@"], 5, false)
    , (["=", "<>", ">", ">=", "<", "<="], 4, true)
    , ([":=", "o"], 3, true)
    , (["before"], 0, true)
    ]

  fun lookup table sym = List.foldl (
    fn ((s, f), NONE) => if Symbol.eq (s, sym) then SOME f else NONE
     | (_, acc)       => acc) NONE table
  fun insert table (sym, fixity) = (sym, fixity)::table
  fun merge t1 t2 = t1 @ t2
  fun trim table = List.foldl (fn ((sym, fixity), acc) =>
    if List.exists (Fn.curry Symbol.eq sym o fst) acc
    then acc
    else (sym, fixity)::acc
  ) [] table

  (* returns (fixit value, is left associative) *)
  val fixityToVal =
    fn Fixity.NONfix => NONE
     | Fixity.INfix (i, _) => SOME (i div 2, i mod 2 = 0)
  fun symOptToVal table symopt =
        symopt
    |> Option.mapPartial (lookup table)
    |> Option.mapPartial fixityToVal

  (* accumulated list is reversed *)
  fun findMin table (item as (symopt,_)) (v, acc) =
    case v of
        NONE => (symOptToVal table symopt, [item])
      | SOME (i, left) => (
        case symOptToVal table symopt of
          NONE => (v, acc)
        | SOME (j, left') =>
          if j < i
          then (SOME (j, left'), [item])
          else if i = j
          (* left beats right associative when conflicting, hence the andalso *)
          then (SOME (i, left andalso left'), item::acc)
          else (v, acc)
      )

  fun minFixitySymbols table args =
    let
      val (v, acc) = List.foldl (Fn.uncurry (findMin table)) (NONE, []) args
    in
      (v, List.rev acc)
    end

  fun findOuterSymbol table [] = NONE
    | findOuterSymbol table args =
      let
        val (fixopt, syms) = minFixitySymbols table args
      in
        case fixopt of
            NONE => SOME (List.hd args)
          | SOME (_, left) => SOME ((if left then List.last else List.hd) syms)
      end

end
