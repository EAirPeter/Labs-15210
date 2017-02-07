functor MkBruteForcePD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
  let
    (* from rec01.pdf *)
    fun parenMatch p =
    let
      fun pm (( NONE , _) | ( SOME 0, CPAREN )) = NONE
        | pm ( SOME c, CPAREN ) = SOME (c -1)
        | pm ( SOME c, OPAREN ) = SOME (c +1)
    in
      iter pm ( SOME 0) p = ( SOME 0)
    end

    fun pup (sq, SOME i, SOME j) =
      if (nth sq i) = OPAREN andalso (nth sq (i + j - 1)) = CPAREN andalso
          parenMatch (subseq sq (i + 1, j - 2))
      then
        SOME (j - 2)
      else
        SOME 0
    fun for2 (sq, SOME i, SOME 0, max) = max
      | for2 (sq, SOME i, SOME j, max) =
          for2 (sq, SOME i, SOME (j - 1), Option210.intMax
            (max, pup (sq, SOME i, SOME j))
          )
    fun for1 (sq, SOME ~1, max) = max
      | for1 (sq, SOME i, max) =
          for1 (sq, SOME (i - 1), (for2 (sq, SOME i, SOME ((length sq) - i), max)))
  in
    if length parens > 0 andalso parenMatch parens then
      for1 (parens, SOME ((length parens) - 1), SOME 0)
    else
      NONE
  end
end

