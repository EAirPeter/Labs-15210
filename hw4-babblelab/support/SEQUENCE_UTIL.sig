signature SEQUENCE_UTIL =
sig
  structure Seq : SEQUENCE

  type 'a seq = 'a Seq.seq
  type 'a ord = 'a Seq.ord
  type 'a hist = ('a * int) seq

  (* Returns a sequence of tokens in string s. Delimiter function is cp. 
   * Typically pass (not o Char.isAlphaNum) for cp.
   *)
  val tokens : (char -> bool) -> string -> string seq

  (* Computes a sequence of tuples (key, n) for each unique key in the
   * input sequence, where n is the number of occurences of the key 
   * in the input.
   *)
  val histogram : 'a ord -> 'a seq -> 'a hist

  (* Selects a value from a histogram; specifically, choose p hist
   * evaluates to the value at p from the cumulative
   * distribution that corresponds to hist, where 0.0 < p < 1.0.
   *)
  val choose : 'a hist -> real -> 'a
end
