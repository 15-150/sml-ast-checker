functor DisableRules (val disable : AllRules.rule list) : RULES =
  Rules
    (local
       val always_disabled =
         [ AllRules.Andalso
         , AllRules.Exceptions
         , AllRules.IsSomeOption
         , AllRules.Modules
         , AllRules.Orelse
         , AllRules.RaiseHandle
         , AllRules.Types
         ]
       val disable = always_disabled @ disable
       val is_disabled = fn r => List.exists (Fn.curry op = r) disable
     in
       val include_rule = not o is_disabled
     end)
