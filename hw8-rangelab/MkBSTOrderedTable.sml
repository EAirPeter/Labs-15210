functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  fun first (T : 'a table) : (key * 'a) option =
    case Tree.expose T
     of NONE => NONE
      | SOME {left = L, key = k, value = v, right = R} =>
        case first L
         of NONE => SOME (k, v)
          | res => res

  fun last (T : 'a table) : (key * 'a) option =
    case Tree.expose T
     of NONE => NONE
      | SOME {left = L, key = k, value = v, right = R} =>
        case last R
         of NONE => SOME (k, v)
          | res => res

  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    last (#1 (Tree.splitAt (T, k)))

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    first (#3 (Tree.splitAt (T, k)))

  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join (L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt (T, k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table = let
    val T1 = case split (T, low)
     of (_, NONE, R) => R
      | (_, SOME v, R) => join (Tree.singleton (low, v), R)
    val T2 = case split (T1, high)
     of (L, NONE, _) => L
      | (L, SOME v, _) => join (L, Tree.singleton (high, v))
  in
    T2
  end

end
