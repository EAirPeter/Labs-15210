functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  fun MST (E : edge seq, n : int) : edge seq = let
    val sqEdgesTmp = Seq.sort
      (fn ((lU, lV, lW), (rU, rV, rW)) => Int.compare (rW, lW))
      E
    (* use edge as label *)
    val sqEdges = Seq.map (fn (u, v, w) => (u, (v, (u, v, w)))) sqEdgesTmp
    (* the UFS *)
    val sqUfs = Seq.tabulate (fn u => u) n
    (* UFS -> LabeledEdges -> CurrentResult -> RandomSeed -> Result *)
    fun boruvka U E liRes wSeed =
      if (Seq.length E) = 0 then liRes
      else let
        val sqMinEdgesTmp = Seq.inject E (
          Seq.tabulate (fn _ => (~1, (~1, ~1, ~1))) n
        )
        val sqMinEdges = Seq.filter
          (fn (_, (u, _)) => u <> ~1)
          (Seq.enum sqMinEdgesTmp)
        val sqRand = Rand.flip wSeed n
        val sqT2H = Seq.filter
          (fn (u, (v, _)) => (nth sqRand u) = 0 andalso (nth sqRand v) = 1)
          sqMinEdges
        val liRes_ =
          (Seq.toList (Seq.map (fn (_, (_, lbl)) => lbl) sqT2H)) @ liRes
        val U_ = Seq.inject (Seq.map (fn (u, (v, _)) => (u, v)) sqT2H) U
        val ufs = nth U_
        val E_ = Seq.filter (fn (u, (v, _)) => u <> v) (
          Seq.map (fn (u, (v, lbl)) => (ufs u, (ufs v, lbl))) E
        )
      in
        boruvka U_ E_ liRes_ (Rand.next wSeed)
      end
  in
    Seq.fromList (boruvka sqUfs sqEdges [] (Rand.fromInt 233))
  end

end
