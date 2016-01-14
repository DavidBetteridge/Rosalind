#if INTERACTIVE
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Utilities.fs"
#else
module Problem12
#endif

open Utilities

let solve (filename:string) (k:int) : string = 
    let fastas = loadFile filename 
    let prefixes = fastas |> List.groupBy (fun f -> (f.Dna.[..(k-1)])) |> Map.ofList
    let directedEdges = fastas 
                        |> List.map (fun f -> let (suffix, label) = (f.Dna.[(f.Dna.Length - k)..], f.Label)
                                              prefixes.TryFind suffix 
                                                  |> Option.map (fun matches -> matches
                                                                                |> List.filter (fun m -> label <> m.Label)
                                                                                |> List.map (fun m -> label + " " + m.Label)))
                        |> List.choose id
                        |> List.collect id
                        |> String.concat "\n"

//    let directedEdges = seq { for s in fastas do 
//                                let suffix = s.Dna.[(s.Dna.Length - k)..]
//                                for t in fastas do
//                                    if s.Label <> t.Label && t.Dna.StartsWith(suffix) then
//                                        yield s.Label + " " + t.Label
//                            }
//                        |> String.concat "\n"

    sprintf "%s" directedEdges


//Rosalind_0498 Rosalind_2391
//Rosalind_0498 Rosalind_0442
//Rosalind_2391 Rosalind_2323


let answer = @"Rosalind_0851 Rosalind_1396
Rosalind_8476 Rosalind_7381
Rosalind_8476 Rosalind_8715
Rosalind_8476 Rosalind_9211
Rosalind_8333 Rosalind_7183
Rosalind_8333 Rosalind_0231
Rosalind_8333 Rosalind_8206
Rosalind_3597 Rosalind_8408
Rosalind_3597 Rosalind_6182
Rosalind_3597 Rosalind_4727
Rosalind_3597 Rosalind_4210
Rosalind_3114 Rosalind_2310
Rosalind_3114 Rosalind_1053
Rosalind_8467 Rosalind_5300
Rosalind_8467 Rosalind_4288
Rosalind_8467 Rosalind_6970
Rosalind_5302 Rosalind_5758
Rosalind_5404 Rosalind_1627
Rosalind_8538 Rosalind_5904
Rosalind_8538 Rosalind_5808
Rosalind_8635 Rosalind_2830
Rosalind_8635 Rosalind_1160
Rosalind_8635 Rosalind_8030
Rosalind_8735 Rosalind_9208
Rosalind_5300 Rosalind_7516
Rosalind_8408 Rosalind_9208
Rosalind_9735 Rosalind_4768
Rosalind_6140 Rosalind_5754
Rosalind_2310 Rosalind_5754
Rosalind_2694 Rosalind_6749
Rosalind_8843 Rosalind_7202
Rosalind_4288 Rosalind_9545
Rosalind_4288 Rosalind_9109
Rosalind_9420 Rosalind_8735
Rosalind_9545 Rosalind_6140
Rosalind_9545 Rosalind_9973
Rosalind_9545 Rosalind_8387
Rosalind_6179 Rosalind_9797
Rosalind_6970 Rosalind_5912
Rosalind_6970 Rosalind_9105
Rosalind_6970 Rosalind_4770
Rosalind_9484 Rosalind_9208
Rosalind_7183 Rosalind_8298
Rosalind_7183 Rosalind_6195
Rosalind_9973 Rosalind_7202
Rosalind_0905 Rosalind_5176
Rosalind_0905 Rosalind_6649
Rosalind_0905 Rosalind_8818
Rosalind_0905 Rosalind_1821
Rosalind_0905 Rosalind_1168
Rosalind_4727 Rosalind_5302
Rosalind_4727 Rosalind_2145
Rosalind_5912 Rosalind_5176
Rosalind_5912 Rosalind_6649
Rosalind_5912 Rosalind_8818
Rosalind_5912 Rosalind_1821
Rosalind_5912 Rosalind_1168
Rosalind_5754 Rosalind_4768
Rosalind_2017 Rosalind_5758
Rosalind_1637 Rosalind_9545
Rosalind_1637 Rosalind_9109
Rosalind_9109 Rosalind_5176
Rosalind_9109 Rosalind_6649
Rosalind_9109 Rosalind_8818
Rosalind_9109 Rosalind_1821
Rosalind_9109 Rosalind_1168
Rosalind_2830 Rosalind_7381
Rosalind_2830 Rosalind_8715
Rosalind_2830 Rosalind_9211
Rosalind_8387 Rosalind_5176
Rosalind_8387 Rosalind_6649
Rosalind_8387 Rosalind_8818
Rosalind_8387 Rosalind_1821
Rosalind_8387 Rosalind_1168
Rosalind_1160 Rosalind_1627
Rosalind_1861 Rosalind_5176
Rosalind_1861 Rosalind_6649
Rosalind_1861 Rosalind_8818
Rosalind_1861 Rosalind_1821
Rosalind_1861 Rosalind_1168
Rosalind_4737 Rosalind_0851
Rosalind_4737 Rosalind_9735
Rosalind_4737 Rosalind_2173
Rosalind_5486 Rosalind_1627
Rosalind_4210 Rosalind_8735
Rosalind_8808 Rosalind_8333
Rosalind_8808 Rosalind_8538
Rosalind_8808 Rosalind_0905
Rosalind_8808 Rosalind_0217
Rosalind_4972 Rosalind_8843
Rosalind_0217 Rosalind_7912
Rosalind_1396 Rosalind_9996
Rosalind_6368 Rosalind_0851
Rosalind_6368 Rosalind_9735
Rosalind_6368 Rosalind_4737
Rosalind_6368 Rosalind_2173
Rosalind_7381 Rosalind_2017
Rosalind_8056 Rosalind_7516
Rosalind_7559 Rosalind_5758
Rosalind_6104 Rosalind_4730
Rosalind_1627 Rosalind_2830
Rosalind_1627 Rosalind_1160
Rosalind_1627 Rosalind_8030
Rosalind_2173 Rosalind_0851
Rosalind_2173 Rosalind_9735
Rosalind_2173 Rosalind_4737
Rosalind_8715 Rosalind_2830
Rosalind_8715 Rosalind_1160
Rosalind_8715 Rosalind_8030
Rosalind_2145 Rosalind_4730
Rosalind_8242 Rosalind_2017
Rosalind_4523 Rosalind_5302
Rosalind_4523 Rosalind_2145
Rosalind_4768 Rosalind_0296
Rosalind_0231 Rosalind_4730
Rosalind_5483 Rosalind_7202
Rosalind_8206 Rosalind_5912
Rosalind_8206 Rosalind_9105
Rosalind_8206 Rosalind_4770
Rosalind_4770 Rosalind_5754
Rosalind_8818 Rosalind_0296
Rosalind_9211 Rosalind_3597
Rosalind_9211 Rosalind_6420
Rosalind_4378 Rosalind_5754
Rosalind_9996 Rosalind_6749
Rosalind_5904 Rosalind_2017
Rosalind_9929 Rosalind_5754
Rosalind_6195 Rosalind_8408
Rosalind_6195 Rosalind_6182
Rosalind_6195 Rosalind_4727
Rosalind_6195 Rosalind_4210
Rosalind_7912 Rosalind_7516
Rosalind_6420 Rosalind_9929
Rosalind_1821 Rosalind_2830
Rosalind_1821 Rosalind_1160
Rosalind_1821 Rosalind_8030
Rosalind_7516 Rosalind_5758
Rosalind_1053 Rosalind_8843
Rosalind_3693 Rosalind_9929
Rosalind_5556 Rosalind_4523
Rosalind_1571 Rosalind_4523
Rosalind_0560 Rosalind_5758
Rosalind_5808 Rosalind_4768
Rosalind_7700 Rosalind_0296
Rosalind_1168 Rosalind_9484
Rosalind_6749 Rosalind_7381
Rosalind_6749 Rosalind_8715
Rosalind_6749 Rosalind_9211"


(solve @"C:\CodeDojo\Rosalind\David_FSharp_Simple\Files\problem12_real.txt" 3) = answer

//solve @"C:\CodeDojo\Rosalind\David_FSharp_Simple\Files\problem12.txt" 3
