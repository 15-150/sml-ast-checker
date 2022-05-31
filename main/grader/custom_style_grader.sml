functor CustomStyleGrader ( structure Rules : RULES
                            val MAX_DEDUCTION : Rational.Int.int
                            val FILES : (string * string) list
                          ) :> GRADER =
struct
  structure G = Grader (open Rules)

  val IMPL = (
    case OS.Process.getEnv "GRADER_IMPL" of
      NONE     => raise Fail "GRADER_IMPL environment variable not set"
    | SOME dir => dir
  )

  val (PATHS, OUTPUT) = ListPair.unzip FILES

  structure Rubric =
    struct

      val description = "Style Grader"

      type t = G.warning list G.result list

      local
        val f = fn filename => fn
          G.Ok (warnings : G.warning list) => String.concat (
            List.map (fn {lines,code,warning,hint,weight} =>
              filename ^ ":" ^ Option.getOpt (lines,"") ^ "\n" ^
              "Style: " ^ warning ^ " (-" ^ Int.toString weight ^ "%)" ^ "\n" ^
              (case code of SOME c => "   code: " ^ c ^ "\n" | NONE => "") ^
              "   hint: " ^ hint ^ "\n"
            ) warnings
          )
        | G.Error s => filename ^ ":\nError: " ^ s ^ "\n"
      in
        val toString = String.concat o Fn.curry (ListPair.map (Fn.uncurry f)) OUTPUT
      end

      infix |>
      fun x |> f = f x

      val score = fn rubric : t => (
        let
          val weight =
            rubric
            |> List.concatMap (fn G.Ok violations => violations | G.Error _ => nil)
            |> List.map #weight
            |> List.foldr (op +) 0
            |> Rational.Int.fromInt
          open Rational
          infix 8 //
        in
          one - Rational.Int.min (weight, MAX_DEDUCTION) // 100
        end)
    end

  val process = fn () => List.map (G.check_style o Fn.curry OS.Path.concat IMPL) PATHS
end
