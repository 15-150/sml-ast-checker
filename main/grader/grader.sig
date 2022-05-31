signature STYLE_GRADER =
  sig
    datatype 'a result = Ok of 'a | Error of string

    type warning = { lines : string option
                   , code : string option
                   , warning : string
                   , hint : string
                   , weight : int
                   }

    val check_style : string -> warning list result

    val toJSON : warning list result -> JSON.value

    val grade : string -> string list -> unit
  end
