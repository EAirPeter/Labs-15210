signature BABBLE =
sig
  structure Rand : RANDOM210
  structure Stats : KGRAM_STATS

  val randomSentence : Stats.kgramstats -> int -> Rand.rand -> string
  val randomDocument : Stats.kgramstats -> int -> Rand.rand -> string
end
