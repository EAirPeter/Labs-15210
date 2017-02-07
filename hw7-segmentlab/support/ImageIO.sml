structure ImageIO :
sig
  structure Seq : SEQUENCE
  type 'a seq = 'a Seq.seq

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  val fromFile: string -> image
  val toFile: string * image -> unit
  val displayImage: image -> unit

  val auxpath: string ref
end = struct
  structure Seq = ArraySequence
  open Seq

  val auxpath = ref "\"./support/"

  type pixel = { r : int, g : int, b : int }
  type image = { width : int, height : int, data : pixel seq seq }

  local
    structure V = Word8Vector

    fun one(s,i) = Word8.toInt(V.sub(s,i));
    fun two(s,i) = one(s,i)+256*one(s,i+1);
    fun four(s,i)= two(s,i)+256*256*two(s,i+2);

    fun mk1 v =  Word8.fromInt (v mod 256)
    fun mk2 v = V.fromList[mk1 v,
                           mk1 (v div 256)];
    fun mk4 v = V.concat[mk2 (v mod 65536),
                         mk2 (v div 65536)];
  in

  fun getPixel ({data, ...}:image) (x, y) = nth (nth data y) x

  fun fromRawFile fileName =
      let val ins = BinIO.openIn fileName
          val header = BinIO.inputN(ins, 4*2)
          val w = four(header,0)
          val h = four(header,4)
          val rgbStream = BinIO.inputN(ins, 3*w*h)
          fun get(x,y) = {r = (one(rgbStream, 3*(x+w*y))),
                          g = (one(rgbStream, 3*(x+w*y) + 1)),
                          b = (one(rgbStream, 3*(x+w*y) + 2))}
          val data = tabulate
            (fn y => tabulate (fn x => get(x,y)) w) h

      in {width=w, height=h, data=data}
      end

  val userID = case OS.Process.getEnv "USER"
                 of SOME user => user
                  | NONE => "noname"

  val magicTag = userID ^ "-" ^ (Time.toString (Time.now()))
  val rawIn = "./tmp/seamIn" ^ magicTag ^ ".raw"
  val rawOut = "./tmp/seamOut" ^ magicTag ^ ".raw"
  val jpgOut = "./tmp/seamOut" ^ magicTag ^ ".jpg"

  fun fromFile fileName =
      let val _ = OS.Process.system
        ("python " ^ !auxpath ^ "/image-feed.py\" " ^ fileName ^ " " ^ rawIn)
      in fromRawFile rawIn
      end

  fun toRawFile (fileName, {width=w, height=h, data=data}:image) =
      let val to_c = fn cv => Word8.fromInt cv
          val pxToB3 = fn {r,g,b} => %[to_c r, to_c g, to_c b]
          val rgbSeq = flatten (map pxToB3 (flatten data))
          val rgbStream = V.tabulate (3*w*h, fn i =>nth rgbSeq i)
          val ous = BinIO.openOut fileName
      in BinIO.output(ous, mk4 w);
         BinIO.output(ous, mk4 h);
         BinIO.output(ous, rgbStream)
      end

  fun toFile (fileName, img) =
      let
        val _ = toRawFile(rawOut, img)
        val _ = OS.Process.system
          ("python " ^ !auxpath ^ "/image-rewrite.py\" " ^ fileName ^ " " ^ rawOut)
      in ()
      end

  fun displayFile fileName = OS.Process.system("image " ^ fileName ^ " &")

  fun displayImage img = (toFile(jpgOut, img); displayFile(jpgOut); ())

  end
end
