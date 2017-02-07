functor MkRefBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  type 'a stseq = 'a STSeq.stseq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = (vertex seq) seq

  (*ONLY FOR TESTING; DON'T COPY THE CODE*)
  fun makeGraph (sfsdc : edge seq) : ugraph = 
    let val gdndbfb = append(sfsdc,(map (fn(i,j)=>(j,i)) sfsdc))
      val asdczx = 1 + reduce Int.max 0 (map Int.max sfsdc)
      val csecds = collect Int.compare gdndbfb
    in inject csecds (tabulate (fn _=> empty()) asdczx) end

  (*ONLY FOR TESTING; DON'T COPY THE CODE*)
  fun findBridges (x351225 : ugraph) : edges = 
    let
      fun x351224 snnvq ((x351227, x351228, x351229, x3512210),O0o0o) =
        if (isSome(STSeq.nth x351228 O0o0o)) then (x351227,x351228,x351229,Int.min(x3512210,valOf (STSeq.nth x351228 O0o0o)))
        else 
          let
            val tncds = STSeq.update (O0o0o,SOME x351229) x351228
            val tasdfsdf = filter (fn O0o0o=> O0o0o<> snnvq) (nth x351225 O0o0o)
            val (inJIbBcSB,inJIbBcSX,inJIbBcSc,inJIbBcSm) = iter (x351224 O0o0o) (x351227,tncds,x351229+1,(length x351225)) tasdfsdf
            val asdf = if snnvq<>O0o0o andalso inJIbBcSm >= x351229 then (STSeq.update (O0o0o,singleton((snnvq,O0o0o))) inJIbBcSB) else inJIbBcSB
          in 
            (asdf,inJIbBcSX,inJIbBcSc,Int.min(x3512210,inJIbBcSm)) 
          end
       val inJIbBcS = tabulate (fn i=>i) (length x351225)
       val x351228 = STSeq.fromSeq (tabulate (fn _=>NONE) (length x351225))
       val x351227 = STSeq.fromSeq (tabulate (fn _=>empty()) (length x351225))
       val vds =  #1(iter (fn(S,O0o0o)=> x351224 O0o0o (S,O0o0o)) (x351227,x351228,0,0) inJIbBcS)
       val AVdsc = flatten(STSeq.toSeq vds)
    in 
      AVdsc
    end
end
