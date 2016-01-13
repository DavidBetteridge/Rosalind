[<AutoOpen>]
module Utilities

open FParsec

type Fasta =
        {
            Label : string
            Dna : string
        }

let loadFile (filename:string) : List<Fasta> = 
    
    let pID = (pstring ">Rosalind_" >>. manySatisfy isDigit ) |>> (fun d ->  "Rosalind_" + d)
    let pDNA = manySatisfy (function | 'A' | 'T' | 'G' | 'C' -> true | _ -> false)
    let pFasta = pipe2 (pID .>> newline) (many (pDNA .>> newline)) (fun label dnas -> {Fasta.Label = label; 
                                                                                       Fasta.Dna = System.String.Concat dnas} )
                                                                                           
    match run (many pFasta) (System.IO.File.ReadAllText(filename)) with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith (sprintf "Parsing the file %s failed with the error %s" filename errorMsg)

let calculateGCContent (dna:string) : double =
    100.0 / (float dna.Length) *
    (dna |> Seq.sumBy (fun c -> if c = 'C' || c = 'G' then 1.0 else 0.0))

//calculateGCContent "AGCTATAG" = 37.5