structure AllRules =
  struct
    datatype rule
    (* checkers *)
      = Andalso
      | Append
      | CaseBool
      | CaseBoolInv
      | CaseOneArm
      | EqualsFalse
      | EqualsList
      | EqualsOption
      | EqualsTrue
      | HashSelector
      | HdTl
      | IfBool
      | IfBoolInv
      | IsSomeOption
      | NestedIfCase
      | NotEqualsFalse
      | NotEqualsTrue
      | Orelse
      | RaiseHandle (* used for constraint in exn/universal *)
      | Record
      | RefUsage
      | While
    (* dec_checkers *)
      | Exceptions
      | Modules
      | Open
      | Types
    (* text_checkers *)
      | Semicolons
    (* hybrid_checkers *)
      | LetValPattern

    val checkers =
      [ (Andalso, Andalso.check, Andalso.warning, Andalso.hint, 5)
      , (Append, Append.check, Append.warning, Append.hint, 5)
      , (CaseBool, CaseBool.check, CaseBool.warning, CaseBool.hint, 5)
      , (CaseBoolInv, CaseBoolInv.check, CaseBoolInv.warning, CaseBoolInv.hint, 5)
      , (CaseOneArm, CaseOneArm.check, CaseOneArm.warning, CaseOneArm.hint, 5)
      , (EqualsFalse, EqualsFalse.check, EqualsFalse.warning, EqualsFalse.hint, 5)
      , (EqualsList, EqualsList.check, EqualsList.warning, EqualsList.hint, 5)
      , (EqualsOption, EqualsOption.check, EqualsOption.warning, EqualsOption.hint, 5)
      , (EqualsTrue, EqualsTrue.check, EqualsTrue.warning, EqualsTrue.hint, 5)
      , (HashSelector, HashSelector.check, HashSelector.warning, HashSelector.hint, 5)
      , (HdTl, HdTl.check, HdTl.warning, HdTl.hint, 5)
      , (IfBool, IfBool.check, IfBool.warning, IfBool.hint, 5)
      , (IfBoolInv, IfBoolInv.check, IfBoolInv.warning, IfBoolInv.hint, 5)
      , (IsSomeOption, IsSomeOption.check, IsSomeOption.warning, IsSomeOption.hint, 5)
      , (NestedIfCase, NestedIfCase.check, NestedIfCase.warning, NestedIfCase.hint, 5)
      , (NotEqualsFalse, NotEqualsFalse.check, NotEqualsFalse.warning, NotEqualsFalse.hint, 5)
      , (NotEqualsTrue, NotEqualsTrue.check, NotEqualsTrue.warning, NotEqualsTrue.hint, 5)
      , (Orelse, Orelse.check, Orelse.warning, Orelse.hint, 5)
      , (RaiseHandle, RaiseHandle.check, RaiseHandle.warning, RaiseHandle.hint, 5)
      , (Record, Record.check, Record.warning, Record.hint, 5)
      , (RefUsage, RefUsage.check, RefUsage.warning, RefUsage.hint, 5)
      , (While, While.check, While.warning, While.hint, 5)
      ]

    val dec_checkers =
      [ (Exceptions, Exceptions.check, Exceptions.warning, Exceptions.hint, 5)
      , (Modules, Modules.check, Modules.warning, Modules.hint, 5)
      , (Open, Open.check, Open.warning, Open.hint, 5)
      , (Types, Types.check, Types.warning, Types.hint, 5)
      ]

    val text_checkers =
      [ (Semicolons, Semicolons.check, Semicolons.warning, Semicolons.hint, 5)
      ]

    val hybrid_checkers =
      [ (LetValPattern, LetValPattern.check, LetValPattern.warning, LetValPattern.hint, 5)
      ]
  end
