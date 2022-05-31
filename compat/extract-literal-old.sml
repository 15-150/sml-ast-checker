structure ExtractLiteral =
  struct

    val int = IntInf.toString
    val word = fn w => "0w" ^ IntInf.toString w
    val real = Fn.id

  end
