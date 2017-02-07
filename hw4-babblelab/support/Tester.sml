structure Tester =
struct
  open StudentTestSuite

  structure Seq = ArraySequence
  structure StringSeqElt = MkSeqElt(structure Elt = StringElt
                                    structure Seq = Seq)
  structure StringSeqBST = MkTreap(structure HashKey = StringSeqElt)
  structure StringSeqTable = MkBSTTable(structure Tree = StringSeqBST
                                        structure Seq = Seq)

  (* Reference solution *)
  structure OurSeqUtil = MkRefSeqUtil(structure S = Seq)
  structure OurKGramStats = MkRefKGramStats(structure Util = OurSeqUtil
                                            structure T = StringSeqTable)

  (* Student solution *)
  structure StuSeqUtil = MkSeqUtil(structure S = Seq)
  structure StuKGramStats = MkTableKGramStats(structure Util = OurSeqUtil
                                              structure T = StringSeqTable)
  structure StuStuKGramStats = MkTableKGramStats(structure Util = StuSeqUtil
                                                 structure T = StringSeqTable)
  structure StuBabble = MkBabble(structure R = Random210
                                 structure KS = StuStuKGramStats
                                 structure Util = StuSeqUtil)

  fun uncurry2 f (h, r) = f h r


  (* * * hackily wrap lookupExts to use the cached stats * * *)
  (* our kgramstats is (#1 o #1) of the input; theirs is (#2 o #1) *)
  fun ourLookupExts ((r,_), kg) =
    case r of
      Result.Value kgss => OurKGramStats.lookupExts kgss kg
    | Result.Exn e => raise e
  fun stuLookupExts ((_,r), kg) =
    case r of
      Result.Value kgss => StuKGramStats.lookupExts kgss kg
    | Result.Exn e => raise e

  val testsChoose =
    List.map (fn (L,r) => (Seq.% L,r)) Tests.testsChoose
  (* cache the generated makeStats output, but don't "test" it *)
  val testsKGramStats =
    let
      fun expandify (s, L) =
        let
          (* our kgramstats is (#1 o #1) of the input; theirs is (#2 o #1) *)
          val kgss = (Result.run (uncurry2 OurKGramStats.makeStats) s,
                      Result.run (uncurry2 StuKGramStats.makeStats) s)
        in
          List.map
            (fn kg => (kgss, Seq.% (String.tokens (not o Char.isAlphaNum) kg)))
            L
        end
    in
      List.concat (List.map expandify Tests.testsKGramStats)
    end
      

  (* * * loggers * * *)
  structure StringIntPairElt = MkPairElt(structure EltA = StringElt
                                      structure EltB = IntElt)
  structure StringIntPairSeqElt = MkSeqElt(structure Elt = StringIntPairElt
                                           structure Seq = Seq)
  structure StringIntPairSeqRealPairElt = MkPairElt(structure EltA = StringIntPairSeqElt
                                                    structure EltB = RealElt)
  val chooseOTS = StringElt.toString
  val chooseITS = StringIntPairSeqRealPairElt.toString
  val chooseLogger = Logger.create (chooseITS, chooseOTS)

  structure StringSeqElt = MkSeqElt(structure Elt = StringElt
                                    structure Seq = Seq)
  fun kGramStatsITS (_ : OurKGramStats.kgramstats Result.result *
                         StuKGramStats.kgramstats Result.result,
                     kg : StuKGramStats.kgram) : string =
    StringSeqElt.toString kg
  val kGramStatsOTS : (StuKGramStats.token * int) Seq.seq -> string = StringIntPairSeqElt.toString
  val kGramStatsLogger = Logger.create (kGramStatsITS, kGramStatsOTS)


  (* * * checkers * * *)
  val chooseChecker = Checker.fromRefsol (uncurry2 StuSeqUtil.choose,
                                          uncurry2 OurSeqUtil.choose,
                                          StringElt.equal)
  val kGramStatsChecker = Checker.fromRefsol (stuLookupExts,
                                              ourLookupExts,
                                              StringIntPairSeqElt.equal)

  fun babbleFromFile fname =
    let
      val shakespeare = TextIO.inputAll (TextIO.openIn fname)
      val kgss = Result.run (uncurry2 StuStuKGramStats.makeStats) (shakespeare, 5)
      val babble =
        case kgss of
          Result.Value kgss => StuBabble.randomDocument kgss 10 (Random210.fromInt 15210)
        | Result.Exn e => "Babble Raise Exception " ^ exnName e ^ "."

      fun lineWrap str =
        if String.size str <= 80 then str else
        String.extract (str, 0, SOME 78) ^ "-\n" ^ lineWrap (String.extract (str, 78, NONE))
    in
      (lineWrap babble) ^ "\n\n"
    end


  (* * * running the tests * * *)
  fun testChoose () =
    Tester.testGroup chooseChecker chooseLogger testsChoose
  fun testKGramStats () =
    Tester.testGroup kGramStatsChecker kGramStatsLogger testsKGramStats
  fun testBabble fname =
    print (babbleFromFile fname)

end
