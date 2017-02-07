functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq

  (* Every word is a vertex. Every pair of synonym is an edge. *)
  type thesaurus = ASP.graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    ASP.makeGraph (Seq.flatten (
      Seq.map
        (
          fn (sWord, sqWords) =>
            Seq.map (fn (sWord_) => (sWord, sWord_)) sqWords
        )
        S
    ))

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    ASP.outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq = let
    val asp = ASP.makeASP T w1
    val sqsqsRes = ASP.report asp w2
  in
    sqsqsRes
  end

end
