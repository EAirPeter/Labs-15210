structure SegTester :
sig
  val makeSegsFile : string * string * int -> unit
  val makeSegsDisplay : string * int -> unit
end = struct
  structure Seq = ArraySequence
  open Seq

  structure Segmenter : SEGMENTER =
    MkBoruvkaSegmenter(structure Seq = Seq structure Rand = Random210)

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  fun segment (img as {width, height, ...}, icredit) =
      if (icredit <= 0)
      then img
      else let
        val graph = ImageGraph.genGraph4 img
        val segmented = Segmenter.findSegments (graph, (width * height)) icredit
        val reconstructed = ForestImage.genImage img segmented
      in
        {width=width, height=height, data=reconstructed}
      end

  fun makeSegsFile (inFile, outFile, k) =
      let
        val imageIn = ImageIO.fromFile inFile
        val imageOut = segment (imageIn, k)
      in
        ImageIO.toFile (outFile, imageOut)
      end

  fun makeSegsDisplay (inFile, k) =
      let
        val imageIn = ImageIO.fromFile inFile
        val imageOut = segment (imageIn, k)
      in
        ImageIO.displayImage imageOut
      end
end
