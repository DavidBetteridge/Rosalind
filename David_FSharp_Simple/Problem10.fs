//module Problem10
#if INTERACTIVE
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Utilities.fs"
#else
module Problem10
#endif

open Utilities

let solve (filename:string) : string = 
    let fastas = loadFile filename
    let firstFasta = fastas |> List.head 
    let dnaLength = firstFasta.Dna.Length

    let columns = [0..(dnaLength - 1)] 
                  |> List.map (fun i -> fastas |> List.map (fun fasta -> fasta.Dna.[i] )) 
                  |> List.map (fun i -> i |> List.countBy id)

    let getCount = function
                   | None -> 0
                   | Some(chr, cnt) -> cnt

    let getLetter letter = 
             List.map (fun c -> c 
                                |> List.tryFind (fun (chr,cnt) -> chr = letter )
                                |> getCount
                                |> string)
             >> String.concat " "
    
    let bestInColumns columns columnNumber : string = 
        columns 
        |> List.item columnNumber 
        |> List.maxBy (fun (_, cnt) -> cnt) 
        |> fst 
        |> (string)

    let consensus = [0..(dnaLength - 1)] 
                    |> List.map (bestInColumns columns)
                    |> String.concat ""

    let profile = ['A';'C';'G';'T'] 
                  |> List.map (fun letter -> letter.ToString() + ": " + (getLetter letter columns))
                  |> String.concat System.Environment.NewLine

    sprintf "%A\n%A" consensus profile

//solve @"C:\CodeDojo\Rosalind\David_FSharp_Simple\Files\problem10.txt"
//
//ATGCAACT
//A: 5 1 0 0 5 5 0 0
//C: 0 0 1 4 2 0 6 1
//G: 1 1 6 3 0 1 0 0
//T: 1 5 0 0 0 1 1 6

//solve @">Rosalind_1
//ATCCAGCT
//>Rosalind_2
//GGGCAACT
//>Rosalind_3
//ATGGATCT
//>Rosalind_4
//AAGCAACC
//>Rosalind_5
//TTGGAACT
//>Rosalind_6
//ATGCCATT
//>Rosalind_7
//ATGGCACT"