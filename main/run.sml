functor Run (Rule : RULES) :
  sig
    datatype 'a result = Ok of 'a | Error of string
    val run : string list -> (int * string) result
  end =
  struct

    datatype 'a result = Ok of 'a | Error of string

    infix |>
    fun x |> f = f x

    structure Check = Grader (open Rule)

    val stringOptToString = fn NONE => "-" | SOME s => s
    val warningToString = fn file => fn {lines,code,warning,hint,weight} =>
        "File: "    ^ file                      ^ "\n"
      ^ "Lines: "   ^ (stringOptToString lines) ^ "\n"
      ^ "Code: "    ^ (stringOptToString code)  ^ "\n"
      ^ "Warning: " ^ warning                   ^ "\n"
      ^ "Hint: "    ^ hint                      ^ "\n\n"
    val resToString =
      fn f =>
        fn Check.Ok xs => List.map f xs |> String.concatWith ""
         | Check.Error s => "#####\nERROR: " ^ s ^ "\n#####\n\n"

    fun foldCheckRes ((file, res), acc) =
      case acc of
        Ok (num, s) =>
          let val str = (s ^ "" ^ resToString (warningToString file) res) in
            case res of
              Check.Ok rs => Ok (num + List.length rs, str)
            | Check.Error _ => Error str
          end
      | Error s => Error (s ^ "" ^ resToString (warningToString file) res)

    fun run inputs =
      let val () = # set CM.Control.verbose false in
        CMSources.all inputs
        |> List.map (fn f => (f, Check.check_style f))
        |> List.foldl foldCheckRes (Ok (0, ""))
      end
  end
