functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  fun findSegments (E, n) initial_credit = let
    val sqEdgesTmp = Seq.sort
      (fn ((lU, lV, lW), (rU, rV, rW)) => Int.compare (rW, lW))
      E
    val sqEdges = Seq.map (fn (u, v, w) => (u, (v, w))) sqEdgesTmp
    val sqUfs = Seq.tabulate (fn u => u) n
    val sqCreds = Seq.tabulate (fn _ => initial_credit) n
    fun boruvka U E C wSeed =
      if (Seq.length E) = 0 then U
      else let
        val sqMinEdgesTmp = Seq.inject E (
          Seq.tabulate (fn _ => (~1, ~1)) n
        )
        val sqMinEdges = Seq.filter
          (fn (_, (v, _)) => v <> ~1)
          (Seq.enum sqMinEdgesTmp)
        val sqRand = Rand.flip wSeed n
        val sqT2H = Seq.filter
          (fn (u, (v, _)) => (nth sqRand u) = 0 andalso (nth sqRand v) = 1)
          sqMinEdges
        val sqH2Ts = Seq.collect Int.compare
          (Seq.map (fn (u, (v, w)) => (v, (u, w))) sqT2H)
        val cred = nth C
        val sqMinC = Seq.map
          (
            fn (u, sqVWs) => (u, Int.min (cred u,
              Seq.reduce Int.min initial_credit
                (Seq.map (fn (v, w) => cred v) sqVWs)
            ))
          )
          sqH2Ts
        val Ctmp = Seq.inject sqMinC C
        val sqNewC = Seq.map
          (
            fn (u, sqVWs) => (u, (nth Ctmp u) - (Seq.reduce Int.+ 0
              (Seq.map (fn (v, w) => w) sqVWs)
            ))
          )
          sqH2Ts
        val C_ = Seq.inject sqNewC Ctmp
        val Unew = Seq.inject (Seq.map (fn (u, (v, _)) => (u, v)) sqT2H) U
        val U_ = Seq.map (nth Unew) Unew
        val ufs = nth U_
        val cred_ = nth C_
        val E_ = Seq.filter
          (fn (u, (v, w)) => u <> v andalso Int.min(cred_ u, cred_ v) >= w)
          (Seq.map (fn (u, (v, w)) => (ufs u, (ufs v, w))) E)
      in
        boruvka U_ E_ C_ (Rand.next wSeed)
      end
  in
    boruvka sqUfs sqEdges sqCreds (Rand.fromInt 233)
  end
end
