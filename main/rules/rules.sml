functor Rules (val include_rule : AllRules.rule -> bool) : RULES =
  struct
    val filter = fn L => List.filter (fn (r, _, _, _, _) => include_rule r) L
    val drop = fn L => List.map (fn (_, w, x, y, z) => (w, x, y, z)) L

    val checkers = drop (filter AllRules.checkers)
    val dec_checkers = drop (filter AllRules.dec_checkers)
    val text_checkers = drop (filter AllRules.text_checkers)
    val hybrid_checkers = drop (filter AllRules.hybrid_checkers)
  end
