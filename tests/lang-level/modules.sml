

structure Test =
struct
  val test = "functions are values"
end

(* NJ extension I believe *)
local
  val x = 0
in
  structure Test2 =
  struct
    val test2 = x
  end
end

(* Another NJ extension *)
local
  structure Test3 =
  struct
    val test3 = x
  end
in
  val _ = Test3.test3
end
