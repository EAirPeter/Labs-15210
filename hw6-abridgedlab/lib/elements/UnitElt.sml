structure UnitElt : ELEMENT =
struct
  type t = int->real

  exception NYI

  val default = Real.fromInt
  fun equal (x,y) = true
  fun compare (x,y) = raise NYI
  fun hash x = raise NYI
  fun toString (_ : t) = "unit"
end