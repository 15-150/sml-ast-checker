fun test x =
  let
    fun inner y = test (y - 1)
  in
    inner x
  end
