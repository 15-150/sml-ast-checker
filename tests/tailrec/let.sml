
fun test1 x =
  let
    fun inner_fact 0 = 1
      | inner_fact n = n * inner_fact (n - 1)
  in
    inner_fact x
  end

fun test2 x =
  let
    fun inner_fact_cps 0 k = k 1
      | inner_fact_cps n k = inner_fact_cps (n - 1) (fn res => k (res * n))
  in
    inner_fact_cps x SOME
  end

fun test3 x =
  let
    fun inner_fact_cps 0 k = k 1
      | inner_fact_cps n k = inner_fact_cps (n - 1) (fn res => k (res * n))
  in
    inner_fact_cps (test3 x)
  end

fun test4 x =
  let
    fun id x = x
  in
    test4 (id x)
  end

fun test5 y =
  let
    fun loop x = loop x
  in
    y
  end

fun test6 x =
  let
    fun inner_fact_cps 0 k = k 1
      | inner_fact_cps n k = inner_fact_cps (n - 1) (fn res => k (res * n))
  in
    test6 (inner_fact_cps x SOME)
  end
