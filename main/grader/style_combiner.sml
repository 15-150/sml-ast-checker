structure StyleCombiner =
  struct
    local
      val fst = fn (x, _) => x
      val snd = fn (_, x) => x

      val format = String.concatWith "\n\n" o (List.map fst)
      val multiply = List.foldr Rational.* Rational.one
    in
      val combine = fn L => (* todo: better name? *)
        (format L, multiply (List.map snd L))
    end
  end
