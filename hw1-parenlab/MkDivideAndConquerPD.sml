functor MkDivideAndConquerPD(structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option =
  let
    fun mp (sq) =
      case showt sq
        of EMPTY => (SOME 0, 0, 0, 0, 0)
         | ELT OPAREN => (SOME 0, 1, 0, 0, 0)
         | ELT CPAREN => (SOME 0, 0, 1, 0, 0)
         | NODE (s1, s2) =>
           let
             val ((m1, lc1, rc1, lp1, rp1), (m2, lc2, rc2, lp2, rp2)) =
               par(fn () => mp s1, fn () => mp s2)
             val mt = if lc1 < rc2 then lc1 else rc2
             val m3 = if lc1 = rc2 then rp2 + lp1 else 0
             val lc3 = lc1 + lc2 - mt
             val rc3 = rc1 + rc2 - mt
             val lp3 = if lc1 > rc2 then lp1 + length s2 else lp2
             val rp3 = if rc2 > lc1 then rp2 + length s1 else rp1
           in
             (Option210.intMax (Option210.intMax (m1, m2), SOME m3),
               lc3, rc3, lp3, rp3)
           end
    val (max, lc, rc, _, _) = mp parens
  in
    if length parens > 0 andalso lc = 0 andalso rc = 0 then
      max
    else
      NONE
  end
end
