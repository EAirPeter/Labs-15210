functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --

  fun x ++ y = BNA.add (x, y)
  fun x -- y = let
    val len = length x
    val z = take (
      (singleton ONE) ++ (
        tabulate
          (
            fn i => if i < 0 orelse i >= length y orelse nth y i = ZERO
            then ONE
            else ZERO
          )
          len
      ) ++ x,
      len
    )
    val c0 = scani
      (fn (a, b) => a + b)
      0
      (map (fn x => if x = ZERO then 1 else 0) z)
    val c = tabulate
      (fn i => (nth c0 (len - 1)) - (if i = 0 then 0 else nth c0 (i - 1)))
      len
  in
    filterIdx (fn (i, _) => len - i > nth c i) z
  end

  val sub = op--
end
