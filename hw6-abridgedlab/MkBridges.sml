functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = vertex seq seq

  fun makeGraph (E : edge seq) : ugraph = let
    val sqRevEdges = Seq.map (fn (u, v) => (v, u)) E
    val sqAllEdges = Seq.append (E, sqRevEdges)
    val sqAdj = Seq.collect Int.compare sqAllEdges
  in
    Seq.map (fn (u, sqVs) => sqVs) sqAdj
  end

  fun findBridges (G : ugraph) : edges = let
    val nVtcs = length G
    val sqEmpty = STSeq.fromSeq (Seq.tabulate (fn _ => ~1) nVtcs)
    fun updmin (sqLhs, nLhs) (sqRhs, nRhs) = STSeq.update
      (nLhs, Int.min (STSeq.nth sqLhs nLhs, STSeq.nth sqRhs nRhs))
      sqLhs
    fun dfs (nDfn, liRes, DFN, LOW, PAR) u = let
      val DFN_ = STSeq.update (u, nDfn) DFN
      val LOW_ = STSeq.update (u, nDfn) LOW
      fun proc ((nDfn, liRes, DFN, LOW, PAR), v) =
        if (STSeq.nth DFN v) = ~1 then let
          val PAR_ = STSeq.update (v, u) PAR
          val (nNewDfn, liNewRes, DFNnew, LOWnew, PARnew) =
            dfs (nDfn + 1, liRes, DFN, LOW, PAR_) v
          val LOW_ = updmin (LOWnew, u) (LOWnew, v)
          val liNewRes_ =
            if (STSeq.nth LOW_ v) > (STSeq.nth DFNnew u) then
              [(u, v)] @ liNewRes
            else
              liNewRes
        in
          (nNewDfn, liNewRes_, DFNnew, LOW_, PARnew)
        end
        else let
          val LOW_ = if v <> (STSeq.nth PAR u) then
            updmin (LOW, u) (DFN, v)
          else
            LOW
        in
          (nDfn + 1, liRes, DFN, LOW_, PAR)
        end
    in
      Seq.iter proc (nDfn, liRes, DFN_, LOW_, PAR) (Seq.nth G u)
    end
    fun proc ((nDfn, liRes, DFN, LOW, PAR), u) =
      if (STSeq.nth DFN u) <> ~1 then
        (nDfn, liRes, DFN, LOW, PAR)
      else
        dfs (nDfn, liRes, DFN, LOW, PAR) u
  in
    Seq.fromList (#2 (
      Seq.iter proc
        (0, [], sqEmpty, sqEmpty, sqEmpty)
        (Seq.tabulate (fn i => i) nVtcs)
    ))
  end

end
