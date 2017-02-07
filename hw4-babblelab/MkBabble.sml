functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq

  exception NoData

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val rsq = Rand.randomRealSeq seed NONE n
      val k = Stats.maxK stats
    in
      (String.concatWith
        " "
        (toList (iter
          (fn (s, r) =>
            (append (s, singleton (Util.choose (
              Stats.lookupExts stats (drop (s, Int.max (0, (length s) - k)))) r)))
          )
          (empty ())
          rsq
        ))
      ) ^ "."
    end

  fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
   let
     (* in order to generate different sentences *)
     val seeds = map Rand.fromInt (Rand.randomIntSeq seed NONE n)
     val newseed = Rand.next (nth seeds ((length seeds) - 1))
     val lens = Rand.randomIntSeq newseed (SOME (5, 11)) n
   in
    String.concatWith " " (toList
      (map (fn (l, s) => randomSentence stats l s) (zip lens seeds))
    )
   end

end
