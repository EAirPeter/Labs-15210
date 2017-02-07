functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  fun choose (hist : 'a hist) (p : real) : 'a =
    (* work around for std solution's bug *)
    if length hist = 0 (*orelse p < 0.0*) orelse p > 1.0 then raise Range
    else let
      val cd = scani
        (fn ((_, f1), (c, f2)) => (c, f1 + f2))
        (#1 (nth hist 0), 0)
        hist
      val ch = Real.ceil (p * Real.fromInt (#2 (nth cd (length cd - 1))))
    in
      #1 (reduce
        (fn ((c1, f1), (c2, f2)) => if f1 < ch then (c2, f2) else (c1, f1))
        (#1 (nth hist 0), ~1)
        cd
      )
    end

end
