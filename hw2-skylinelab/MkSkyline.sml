functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
    case showt buildings of
      EMPTY => empty ()
    | ELT (l, h, r) => fromList [(l, h), (r, 0)]
    | NODE (s1, s2) => let
        fun combine (sl, sr) =
          if length sl = 0 then sr
          else if length sr = 0 then sl
          else let
            fun cmp (lhs : (int * int), rhs : (int *int)) =
              if #1 lhs < #1 rhs then LESS else GREATER
            fun neginf (sq : (int *int) seq) = map (fn (p) => (#1 p, ~1)) sq
            fun side (sq) = scani
              (fn (lhs, rhs) => if #2 rhs > ~1 then rhs else (#1 rhs, #2 lhs))
              (0, ~1) sq
            val sl' = side (merge cmp sl (neginf sr))
            val sr' = side (merge cmp sr (neginf sl))
          in
            map (fn (lhs, rhs) => (#1 lhs, Int.max (#2 lhs, #2 rhs)))
              (zip sl' sr')
          end
        val (s1', s2') = par(fn () => skyline s1, fn () => skyline s2)
        val s3 = combine(s1', s2')
      in
        filterIdx (fn (i, p) =>
          if i = 0 then true else #2 (nth s3 (i - 1)) <> #2 p)
          s3
      end
end
