#if INTERACTIVE
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#r @"C:\CodeDojo\Rosalind\David_FSharp_Simple\packages\Http.fs.1.5.1\lib\net40\HttpClient.dll"
#load "Utilities.fs"
#else
module Problem_16
#endif

open Utilities
open HttpClient
open System.Text.RegularExpressions

let solve (filename:string) : string = 

    let getFasta uniprot_id : Fasta =  
        let url = sprintf "http://www.uniprot.org/uniprot/%s.fasta" uniprot_id
        let page = (createRequest Get url |> getResponseBody)
        page |> parseFasta


    let findLocationsOfPattern pattern input : seq<int> =
        seq {
           for mtch in Regex.Matches(input,pattern) do
                yield mtch.Index + 1 }        

    let seqToString (nums:seq<int>) : string =
        nums |> Seq.fold (fun s t -> (if s = "" then "" else s + " ") + t.ToString()) ""

    let pattern = "N(?=[^P][ST][^P])"

    let loadFile (filename:string) : string[] =
        System.IO.File.ReadAllLines filename 

    loadFile filename 
    |> Array.map (fun id -> id, getFasta id)
    |> Array.map (fun (id, f) -> id, (findLocationsOfPattern pattern f.Dna) |> seqToString)
    |> Array.filter (fun (_, l) -> l <> "")
    |> Array.map (fun (id, positions) -> id + "\r\n" + positions)
    |> String.concat "\r\n"


printfn "%A" ((solve @"C:\CodeDojo\Rosalind\David_FSharp_Simple\Files\rosalind_mprt.txt"))

