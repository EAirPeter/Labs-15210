functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq
 
  type 'a seq = 'a Seq.seq 

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }
  type gradient = real

  fun generateGradients ({width, height, data}:image):gradient seq seq =
    tabulate
      (
        fn i => tabulate
          (
            fn j => if i = height - 1 then 0.0
            else if j = width - 1 then Real.posInf
            else let
              fun rgb i j = nth (nth data i) j
              fun difsqr f a b = (f a - f b) * (f a - f b)
              fun dif (p1 : pixel) (p2 : pixel) =
                difsqr #r p1 p2 + difsqr #g p1 p2 + difsqr #b p1 p2
              val pix = rgb i j
              val pixr = rgb i (j + 1)
              val pixd = rgb (i + 1) j
            in
              Math.sqrt (dif pix pixr + dif pix pixd)
            end
          )
          width
      )
      height

  fun findSeam (G:gradient seq seq) : int seq = let
    val N = length G
    val M = length (nth G 0)
    fun min (jh1 : int * real, jh2 : int * real) =
      if #2 jh1 < #2 jh2 then jh1 else jh2
    fun min3 (jh1, jh2, jh3) = min (min (jh1, jh2), jh3)
    fun f (liJhs, i) = if i = N then liJhs else let
      fun h j = if j < 0 orelse j >= M then
        Real.posInf
      else
        #2 (nth (hd liJhs) j)
      fun jh j = (j, h j)
      fun g j_ (j, h) = (j, h + nth (nth G i) j_)
      val sqG = tabulate (fn j => g j (min3 (jh (j - 1), jh (j + 1), jh j))) M
    in
      f ([sqG] @ liJhs, i + 1)
    end
    val liJhs = f ([tabulate (fn _ => (~1, 0.0)) M], 0)
    val sqJhs = take (fromList liJhs, N)
    val minY = #1 (reduce
      (
        fn (j_jh1, j_jh2) =>
          if #2 (#2 j_jh1) < #2 (#2 j_jh2) then j_jh1 else j_jh2
      )
      (~1, (~1, Real.posInf))
      (enum (nth sqJhs 0))
    )
    val liRes = iter
      (fn (liY, sqJh) => [#1 (nth sqJh (hd liY))] @ liY)
      [minY]
      sqJhs
  in
    drop (fromList liRes, 1)
  end

end
