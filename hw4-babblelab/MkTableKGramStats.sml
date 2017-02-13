functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq

  type token = string
  type kgram = token seq

  (* record maxK and the histogram for every kgram *)
  type kgramstats = int * (string hist Table.table)

  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let
      fun cmp (kg1, kg2) = collate String.compare (kg1, kg2)
      val toks = tokens (not o Char.isAlphaNum) corpus
      fun f k = tabulate
        (fn i => (subseq toks (i, k), nth toks (i + k)))
        (length toks - k)
      val keys = collect cmp (flatten (tabulate f (maxK + 1)))
      val hists = map (fn (kg, sq) => (kg, histogram String.compare sq)) keys
    in
      (maxK, Table.fromSeq hists)
    end

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq =
    case Table.find (#2 stats) kgram of
      NONE => empty ()
    | SOME hs => hs

  fun maxK (stats : kgramstats) : int =
    #1 stats

end
