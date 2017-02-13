structure Tests =
struct

  (* Do not remove the following line! *)
  val corpus = TextIO.inputAll (TextIO.openIn "corpus.txt")

  val testsChoose : (((string * int) list) * real) list  = [
    ([("test", 10)], 0.5),
    ([("test", 2), ("awesome", 2)], 0.5),
    ([("yay", 1), ("woah", 2), ("oh", 3), ("yup", 4)], 0.47),
    (* should output "sore" since 0.77 < (12 + 22) / (12 + 22 + 10) *)
    ([("kore", 12), ("sore", 22), ("are", 10)], 0.77),
    (* should output "ati" since 0.78 > (12 + 22) / (12 + 22 + 10) *)
    ([("koti", 12), ("soti", 22), ("ati", 10)], 0.78),
    (* an empty sequence, should raise exception *)
    ([], 0.3),
    (* a negative p, should raise exception, however std solution did not *)
    ([("gg", 20), ("wp", 10)], ~0.3),
    (* p > 1, should raise exception *)
    ([("gl", 333), ("hf", 494)], 1.01)
  ]

  (* You must add test kgrams for the corpus in corpus.txt as part of task 5.5
   * You may edit corpus.txt -- it will be handed in.
   *
   * You may also add other tests, which use other corpi (corpuses?), but those
   * corpuses will not be submitted. *)
  val testsKGramStats : ((string * int) * (string list)) list = [
    ((corpus, 5),
        ["direction",
         "time",
         "direction of time",
         "would write",
         "What Eddington says about",
         (* for general testing *)
         "in reverse",
         "this",
         "Eddington says about the",
         (* an empty corpus *)
         ""
        ])
  ]


end
