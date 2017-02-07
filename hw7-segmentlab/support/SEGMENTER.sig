signature SEGMENTER =
sig
  structure Seq : SEQUENCE

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* findSegments edgeSet vertexCount credits
   *   Computes a segmentation of the input graph
   * represented by a sequence mapping vertices
   * to the canonical vertex for their segment. *)
  val findSegments : edge Seq.seq * int -> int -> vertex Seq.seq
end
