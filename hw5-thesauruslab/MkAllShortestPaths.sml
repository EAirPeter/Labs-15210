functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  (* adjacency map * number of edges * number of vertices *)
  type graph = (Set.set table) * int * int
  (* maps vertex to its parents on bfs tree *)
  type asp = vertex seq table

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph = let
    val mAdj = Table.map Set.fromSeq (Table.collect E)
    val nEdgs = Seq.length E
    val sqRange = Seq.flatten (Seq.map Set.toSeq (Table.range mAdj))
    val nVtcs = Set.size (
      Set.union (Table.domain mAdj, Set.fromSeq sqRange)
    )
  in
    (mAdj, nEdgs, nVtcs)
  end


  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    #2 G

  fun numVertices (G : graph) : int =
    #3 G

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case Table.find (#1 G) v
     of NONE => Seq.empty ()
      | SOME (stAdj) => Set.toSeq stAdj

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp = let
    fun bfs (mRes : asp) (sqCur : vertex seq) : asp * vertex seq =
      if (Seq.length sqCur) = 0 then (mRes, sqCur)
      else let
        val mCurRes = Table.collect (Seq.flatten (
          Seq.map
            (fn u => Seq.map (fn v => (v, u))
            (outNeighbors G u))
            sqCur
        ))
        val mNewRes = Table.merge (fn (sqPar, _) => sqPar) (mRes, mCurRes)
        val sqNewCur = Set.toSeq (Table.domain (
          Table.erase (mCurRes, Table.domain mRes)
        ))
      in
        bfs mNewRes sqNewCur
      end
    in
      #1 (bfs (Table.singleton (v, Seq.empty ())) (Seq.singleton v))
    end

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq = let
    fun dfs (v : vertex) : vertex list seq =
      case Table.find A v
       of NONE => Seq.empty ()
        | SOME (sqPars) =>
          if (Seq.length sqPars) = 0 then Seq.singleton [v]
          else
            Seq.map (fn liVtcs => [v] @ liVtcs) (
              Seq.flatten (Seq.map (fn u => dfs u) sqPars)
            )
  in
    Seq.map (fn liVtcs => Seq.rev (Seq.fromList liVtcs)) (dfs v)
  end

end
