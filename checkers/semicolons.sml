structure Semicolons =
  struct
    fun uncommented_semicolon _ _ [] = false
      | uncommented_semicolon 0 _ [x] = x = #";"
      | uncommented_semicolon _ _ [_] = false
      | uncommented_semicolon n str (x :: y :: xs) =
        case (x, y) of
          (#"(", #"*") =>
            if str then
              uncommented_semicolon n str xs
            else
              uncommented_semicolon (n + 1) str xs
        | (#"\"", _) =>
            if n > 0 then
              uncommented_semicolon n str (y :: xs)
            else
              uncommented_semicolon n (not str) (y :: xs)
        | (#";", _) =>
            if n = 0 andalso not str then
              true
            else
              uncommented_semicolon n str (y :: xs)
        | (_, #";") =>
            if n = 0 andalso not str then
              true
            else
              uncommented_semicolon n str xs
        | (#"*", #")") =>
            if str then
              uncommented_semicolon n str xs
            else if n > 0 then
              uncommented_semicolon (n - 1) str xs
            else
              uncommented_semicolon 0 str xs
        | (_, #"(") => uncommented_semicolon n str (y :: xs)
        | (_, #"*") => uncommented_semicolon n str (y :: xs)
        | (#"\\", #"\"") => uncommented_semicolon n str xs
        | (_, #"\"") =>
            if n > 0 then
              uncommented_semicolon n str xs
            else
              uncommented_semicolon n (not str) xs
        | (_, _) => uncommented_semicolon n str xs

    fun check text = uncommented_semicolon 0 false (String.explode text)

    val warning = "Semicolons in code."
    val hint =
      "semicolons should only be used in the REPL or in imperative code"

  end
