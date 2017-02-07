functor MkRefKGramStats(structure Util : SEQUENCE_UTIL) : KGRAM_STATS =
struct
  structure Seq = Util.Seq
  open Util
  open Seq

  type token = string
  type kgram = token seq

  type kgramstats = kgram * int

  (* This doesn't do anything to process the token seq, and is incorrect.
   * It should only be used to test against students' solutions. *)
  fun makeStats (corpus : string) (maxK : int) : kgramstats =
      (tokens (fn c => not (Char.isAlphaNum c)) corpus, maxK)

  (* logical AND *)
  fun both (p : bool, q : bool) : bool = p andalso q

  fun lookupExts ((stats, k) : kgramstats) (kgram : kgram) : (token * int) seq =
      if length kgram > k then (empty ()) else
      let
        val slen = length stats
        val len = length kgram

        fun strEq t = case String.compare t of EQUAL => true | _ => false

        fun isMatchAt ind =
            let
              val indStats = drop (stats, ind)
            in
              if (length indStats) < (len + 1) then false
              else reduce both true (map2 strEq kgram indStats)
            end
        fun getExt ind =
            if   isMatchAt ind
            then SOME (nth stats (ind+len))
            else NONE

        val extS = map valOf (filter isSome (tabulate getExt slen))
          handle Range => (empty ())
      in
        histogram String.compare extS
      end

  fun maxK (stats, k) = k

end
