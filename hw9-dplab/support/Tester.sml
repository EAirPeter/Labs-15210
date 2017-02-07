structure Tester :
sig
  val removeSeamsFile : string * string * int -> unit
  val removeSeamsDisplay : string * int -> unit
end
=
struct
  structure Seq = ArraySequence
  open Seq
  
  structure SeamFind = MkSeamFind(structure Seq = Seq) : SEAMFIND

  type pixel = { r : real, g : real, b : real }
  type image = { width : int, height : int, data : pixel seq seq }

  fun removeSeam (img as {width, height, data} : image, seam) =
      let
         fun deleteIth(S,i) =
           Seq.tabulate (fn j => Seq.nth S (if (j < i) then j else j+1))
                        (width-1)
         val newData = Seq.map2 deleteIth data seam
      in
         {width=width-1, height=height, data = newData}
      end

  (* Removes k seams from an image *)
  fun removeSeams (img, k) =
      if (k <= 0) then img
      else removeSeams (removeSeam (img, SeamFind.findSeam 
                                         (SeamFind.generateGradients img)), k-1)

  fun removeSeamsFile (inFile, outFile, k) = 
      let
        val imageIn = ImageIO.fromFile(inFile)
        val imageOut = removeSeams(imageIn, k)
      in
        ImageIO.toFile(outFile, imageOut)
      end

  fun removeSeamsDisplay (inFile, k) = 
      let
        val imageIn = ImageIO.fromFile(inFile)
        val imageOut = removeSeams(imageIn, k)
      in
        ImageIO.displayImage(imageOut)
      end
end
