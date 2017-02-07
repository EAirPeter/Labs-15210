functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y =
    if length x < length y then y ** x
    else if length y = 0 then empty ()
    else if length y = 1 then x
    else let
      fun wrap z = let
        val k = scani Int.+ 0 (map (fn e => if e = ZERO then 1 else 0) z)
      in
        filterIdx
          (
            fn (i, _) =>
              (length k) - i > (nth k ((length k) - 1)) -
              (if i = 0 then 0 else nth k (i - 1))
          )
          z
      end
      val n = (length x) div 2
      val lx = wrap (take (x, n))
      val hx = drop (x, n)
      val ly = if n > length y then y else wrap (take (y, n))
      val hy = if n > length y then empty () else drop (y, n)
      val a0n = lx ** ly
      val a2n = hx ** hy
      val a1n = (lx ++ hx) ** (ly ++ hy) -- a0n -- a2n
    in
      (append (tabulate (fn _ => ZERO) (n * 2), a2n)) ++
        (append (tabulate (fn _ => ZERO) n, a1n)) ++
        a0n
    end

  val mul = op**
end
