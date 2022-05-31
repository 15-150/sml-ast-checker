functor Grader (include RULES) :> STYLE_GRADER =
struct

  fun sourceloc_to_string (left : SourceMap.sourceloc, right : SourceMap.sourceloc) =
      let
        val left_line = #line left
        val right_line = #line right
        val left_str = Int.toString left_line ^ "." ^ Int.toString (#column left)
        val right_str = if left_line = right_line
                        then Int.toString (#column right)
                        else Int.toString right_line ^ "." ^ Int.toString (#column right)
      in
        left_str ^ "-" ^ right_str
      end

  fun region_to_string sourceMap (left, right) : string =
      let
        val regions = SourceMap.fileregion sourceMap (left, right)
      in
        concat (map sourceloc_to_string regions)
      end

  datatype 'a result = Ok of 'a | Error of string

  type warning = {
    lines   : string option,
    code    : string option,
    warning : string,
    hint    : string,
    weight  : int
    }

  val indent = "         "

  fun problems runchecker (source : Source.inputSource, ast, env) (check, warning, hint, weight) =
    let
      val filename = #fileOpened source
      val sourceMap = #sourceMap source
      val (_, found) = runchecker (fn _ => [], check) ast
      fun format (exp, region_opt) = {
        lines   = Option.map (region_to_string sourceMap) region_opt,
        code    = SOME (Lint.pp_exp exp env indent),
        warning = warning,
        hint    = hint,
        weight  = weight
      }
    in
      List.map format found
    end

  fun dec_problems runchecker (source : Source.inputSource, ast, env) (check, warning, hint, weight) =
    let
      val filename = #fileOpened source
      val sourceMap = #sourceMap source
      val (found, _) = runchecker (check, fn _ => []) ast
      fun format (dec, region_opt) = {
        lines   = Option.map (region_to_string sourceMap) region_opt,
        code    = SOME (Lint.pp_dec dec env indent),
        warning = warning,
        hint    = hint,
        weight  = weight
      }
    in
      List.map format found
    end

  fun all_input fs =
      case TextIO.inputLine fs of
          NONE => ""
        | SOME s => s ^ all_input fs

  fun text_problems (filename, text) (check, warning, hint, weight) =
    if check text
      then [{
        lines   = NONE,
        code    = NONE,
        warning = warning,
        hint    = hint,
        weight  = weight
      }]
      else []

  fun hybrid_problems text (source : Source.inputSource, ast, env) (check, warning, hint, weight) =
    let
      val filename = #fileOpened source
      val sourceMap = #sourceMap source
      val found = check text ast
      fun format (exp, region_opt) = {
        lines   = Option.map (region_to_string sourceMap) region_opt,
        code    = SOME (Lint.pp_exp exp env indent),
        warning = warning,
        hint    = hint,
        weight  = weight
      }
    in
      List.map format found
    end

  fun check_style filename =
    let
      val source = Parse.getSource filename
      val ast = SmlFile.parse source
      val env = Parse.getEnv ast
      val text = all_input (TextIO.openIn filename)
    in
      Ok (
        List.concat (
          List.map (text_problems (filename, text)) text_checkers
          @ List.map (problems AstTraverse.find_dec (source, ast, env)) checkers
          @ List.map (dec_problems AstTraverse.find_dec (source, ast, env)) dec_checkers
          @ List.map (hybrid_problems text (source, ast, env)) hybrid_checkers
        )
      )
    end
    handle IO.Io _              => Error "File is missing."
         | CompileExn.Compile _ => Error "Syntax error found in file."

  local
    val stringOptToJSON = fn
      NONE   => JSON.NULL
    | SOME s => JSON.STRING s
    val warningToJSON = fn {lines,code,warning,hint,weight} => JSON.OBJECT [
      ("lines"  , stringOptToJSON lines           ),
      ("code"   , stringOptToJSON code            ),
      ("warning", JSON.STRING warning             ),
      ("hint"   , JSON.STRING hint                ),
      ("weight" , JSON.INT (IntInf.fromInt weight))
    ]
  in
    val toJSON = fn
      Ok warnings => JSON.ARRAY (List.map warningToJSON warnings)
    | Error s     => JSON.STRING s
  end


  fun grade output files =
    let
      val json = JSON.ARRAY (List.map (toJSON o check_style) files)
      val out = TextIO.openOut output
    in
      JSONPrinter.print (out,json) before TextIO.closeOut out
    end

end
