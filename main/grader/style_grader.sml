functor StyleGrader ( val disable : AllRules.rule list
                      val FILES : (string * string) list
                    ) :> GRADER = CustomStyleGrader(
    structure Rules = DisableRules(val disable = disable)
    val MAX_DEDUCTION : Rational.Int.int = 15
    val FILES = FILES
  )
