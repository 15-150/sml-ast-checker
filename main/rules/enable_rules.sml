functor EnableRules (val enable : AllRules.rule list) : RULES =
  Rules (val include_rule = fn r => List.exists (Fn.curry op = r) enable)
