functor MkRefSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  fun tokens (cp : char -> bool) (str : string) : string seq =
      % (String.tokens cp str)

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  (* This is a bit ridiculous - don't try to understand or imitate it. It is
   * here only for testing against and is extremely slow. *)
  fun choose (hist : 'a hist) (p : real) : 'a =
      let
        fun tupToSeq (x,n) = tabulate (fn _ => x) n
        val unhist = flatten (map tupToSeq hist)

        val ind = Real.ceil (p * (Real.fromInt (length unhist))) - 1
        val ind' = if ind < 0 then 0 else ind
      in
        nth unhist ind'
      end

end
