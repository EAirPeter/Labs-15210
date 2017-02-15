functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable

  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = unit table table

  fun makeCountTable (S : point seq) : countTable = let
    val sqSorted = Seq.sort (fn (p1, p2) => compareKey (#1 p1, #1 p2)) S
    val (sqMapsTmp, mLast) = Seq.iterh
      (fn (m, p) => OrdTable.insert #1 (#2 p, ()) m)
      (OrdTable.empty ())
      sqSorted
    val sqMaps =
      if (Seq.length S) = 0 then Seq.empty ()
      else Seq.append (Seq.drop (sqMapsTmp, 1), Seq.singleton mLast)
    val sqCounts = Seq.map (fn (p, m) => (#1 p, m)) (Seq.zip sqSorted sqMaps)
  in
    OrdTable.fromSeq sqCounts
  end

  fun count (T : countTable)
    ((xLeft, yHi) : point, (xRght, yLo) : point) : int =
  let
    val L = case OrdTable.previous T xLeft
     of NONE => OrdTable.empty ()
      | SOME (k, v) => v
    val R = case OrdTable.split (T, xRght)
     of (_, SOME v, _) => v
      | (L, NONE, _) =>
        case OrdTable.last L
         of NONE => OrdTable.empty ()
          | SOME (k, v) => v
    fun proc T = OrdTable.size (OrdTable.getRange T (yLo, yHi))
  in
    (proc R) - (proc L)
  end
end
