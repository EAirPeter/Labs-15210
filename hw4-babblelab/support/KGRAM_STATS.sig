signature KGRAM_STATS =
sig
  structure Seq : SEQUENCE

  type kgramstats
  type token = string
  type kgram = token Seq.seq

  val makeStats : string -> int -> kgramstats
  val lookupExts : kgramstats -> kgram -> ((token * int) Seq.seq)
  val maxK : kgramstats -> int
end
