structure Dec =
  struct

    val ovld = Fn.id

    val legacy = fn _ => fn _ => raise Fail "Invalid case"

  end
