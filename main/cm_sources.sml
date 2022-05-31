structure CMSources :
  sig
    val all : string list -> string list
  end =
  struct

    infix |>
    fun x |> f = f x

    fun getSources f =
      Option.map (List.map # file o List.filter (fn x => # class x = "sml"))
        (CM.sources NONE f)

    fun isCM f = if String.isSuffix ".cm" f then SOME f else NONE
    fun isSML f =
      let fun check ft = String.isSuffix ft f in
        if check ".sml" orelse check ".fun" orelse check ".sig" then
          SOME f
        else
          NONE
      end

    fun getCMSources sources =
      sources
      |> List.mapPartial isCM
      |> List.mapPartial getSources
      |> List.concat
    val getSMLSources = List.mapPartial isSML

    fun removeDups (L : string list) =
      case L of
        [] => []
      | x :: xs => x :: removeDups (List.filter (not o Fn.curry op = x) xs)

    fun all sources =
      removeDups (getCMSources sources @ getSMLSources sources)
  end
