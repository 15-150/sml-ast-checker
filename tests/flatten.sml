fun flatten l =
  case l of
    [] => []
  | []::xss => flatten xss
  | (x::xs)::xss => x::flatten (xs::xss)


val [] = []
