functor MkSequentialPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
      let
        (* pd : (int list * int option) option * (int * paren)
         *   -> (int list * int option) option
         *
         * If parens is a well-formed parenthesis sequence, then
         * pd maintains the state (SOME (opens, max)) while iterating
         * across the enumerated paren sequence, where:
         *
         *   (opens : int list) lists the positions of all unclosed OPAREN
         *   (max : int option) maintains the maximum paren dist found so far
         *)
        fun pd (NONE, _) = NONE
          | pd (SOME ([], _), (_, CPAREN)) = NONE
          | pd (SOME (opens, max), (i, OPAREN)) = SOME (i::opens, max)
          | pd (SOME (j::opens, max), (i, CPAREN)) =
            SOME (opens, Option210.intMax (max, SOME (i-j-1)))
      in
        case iter pd (SOME ([], NONE)) (enum parens)
          of SOME ([], max) => max
           | _ => NONE
      end
end
