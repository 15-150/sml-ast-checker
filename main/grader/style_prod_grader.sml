functor StyleProdGrader2 (
  val description : string
  structure Grader1 : GRADER
  structure Grader2 : GRADER
  val combine : (string * Rational.t) list -> (string * Rational.t)
) :> GRADER =
  struct
    structure Rubric =
      struct
        val description = description

        type t = {
          g1 : Grader1.Rubric.t,
          g2 : Grader2.Rubric.t
        }

        local
          val fst = fn (x,_) => x
          val snd = fn (_,x) => x

          val strings = fn {g1=g1,g2=g2} => [
            Grader1.Rubric.toString g1,
            Grader2.Rubric.toString g2
          ]
          val scores = fn {g1=g1,g2=g2} => [
            Grader1.Rubric.score g1,
            Grader2.Rubric.score g2
          ]

          val combined = fn rubric =>
            combine (ListPair.zip (strings rubric, scores rubric))
        in
          val toString = fst o combined

          val score = snd o combined
        end
      end

    val process = fn () => {
      g1 = Grader1.process (),
      g2 = Grader2.process ()
    }
  end
