structure Top :
  sig
    exception Error

    val run : string list -> int
    val export : unit -> unit
  end =
  struct

    exception Error

    structure AllChecks =
      Run (Rules (val include_rule : AllRules.rule -> bool = Fn.const true))

    local
      open AllRules
    in
      structure MainChecks = Run(
        EnableRules(
          val enable =
            [ HashSelector
            , Record
            , RefUsage
            , While
            , Semicolons
            , RaiseHandle
            , Exceptions
            , Modules
            , Open
            , Types
            ]))
    end

    structure Use = MainChecks

    fun run inputs =
      case Use.run inputs of
        Use.Ok (num, s) =>
          ( print s
          ; print ("\n\nFound " ^ (Int.toString num) ^ " issues.\n\n")
          ; num
          )
      | Use.Error s =>
          (print s; print ("\n\nTerminated with error.\n\n"); raise Error)

    fun main (prog_name, args) =
      (if run args = 0 then
         OS.Process.success
       else
         OS.Process.failure)
        handle Error => OS.Process.failure

    fun export () = SMLofNJ.exportFn ("limit-sml", main)

  end
