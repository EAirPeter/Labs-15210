signature MST =
sig
  structure Seq : SEQUENCE

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* Computes a minimum spanning tree of the undirected graph
   * represented by a sequence of directed edges *)
  val MST : (edge Seq.seq * int) -> edge Seq.seq
end
