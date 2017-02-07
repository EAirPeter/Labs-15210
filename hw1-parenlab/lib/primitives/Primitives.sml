structure Primitives : PRIMITIVES =
struct
  (* Parallelism can't ordinarily be expressed in SML, so we introduce these
   * primitives. Do not rely their implementations being sequential. *)
  fun par (f, g) = (f (), g ())
  fun par3 (f, g, h) =  (f (), g (), h ())
  fun parTab (n, f) = let val v = Vector.tabulate (n, f)
                      in fn i => Vector.sub (v, i) end
end
