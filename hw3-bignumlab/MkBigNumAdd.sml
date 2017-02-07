functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++

  datatype carry = GEN | PROP | STOP

  fun x ++ y = let
    val len = Int.max (length x, length y)
    fun nth' li i =
      if i < length li andalso i >= 0 andalso (nth li i) = ONE then 1
      else 0
    val h = append (
      tabulate
        (
          fn (i) => case (nth' x i) + (nth' y i) of
            2 => GEN | 1 => PROP | _ => STOP
        )
        len,
      singleton STOP
    )
    val g = append (
      singleton STOP,
      scani
        (fn (g', h') => if h' = PROP then g' else h')
        STOP
        (take (h, len))
    )
    val f = map
      (
        fn (g', h') =>
          if (if g' = GEN then 1 else 0) + (if h' = PROP then 1 else 0) = 1
          then ONE
          else ZERO
      )
      (zip g h)
  in
    if nth f len = ZERO then take (f, len) else f
  end

  val add = op++
end
