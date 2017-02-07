functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real

  (* Define this type yourself *)
  type graph = (vertex * weight) Seq.seq Table.table

  fun makeGraph (E : edge Seq.seq) : graph =
    Table.collect (Seq.map (fn (u, v, w) => (u, (v, w))) E)

  fun findPath h G (S, T) = let
    fun bfs (D : real Table.table) (Q : vertex PQ.pq) : real Table.table =
      case PQ.deleteMin Q
       of (NONE, _) => D
        | (SOME (f, u), Q_) =>
          case Table.find D u
           of SOME _ => bfs D Q_
            | NONE => let
                val fDis = f - (h u)
                val Dnew = Table.insert (fn (f_, _) => f_) (u, fDis) D
                val sqVws = case Table.find G u
                 of NONE => Seq.empty ()
                  | SOME res => res
                val Qnew = Seq.iter
                  (
                    fn (Q, (v, w)) => PQ.insert (fDis + w + (h v), v) Q
                  )
                  Q_
                  sqVws 
              in
                bfs Dnew Qnew
              end
    val mResBfs = bfs (Table.empty ()) (PQ.fromList (Seq.toList (
      Seq.map (fn u => (h u, u)) (Set.toSeq S)
    )))
    val mRes = Table.extract (mResBfs, T)
    fun proc (NONE, rhs) = SOME rhs
      | proc (SOME (vLhs, fLhs), (vRhs, fRhs)) =
        if fLhs < fRhs then SOME (vLhs, fLhs)
        else SOME (vRhs, fRhs)
  in
    Seq.iter proc NONE (Table.toSeq mRes)
  end

end
