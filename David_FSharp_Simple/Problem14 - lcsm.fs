#if INTERACTIVE
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#load "Utilities.fs"
#else
module Problem14
#endif

open Utilities

let solve (filename:string) = // : string = 
    
    // Extract the two shortest fastas
    let splitUpFastas fastas =
         let ordered = fastas |> List.sortBy (fun f -> f.Dna.Length)
         let shortest = ordered.Head
         let secondShortest = ordered.Tail.Head
         let rest = ordered.Tail.Tail 
         (shortest, secondShortest, rest)


    //ABCD -> DABC
    let rotate (dna:char[]) : char[] =
        Array.init dna.Length (function
                               | 0 -> dna.[dna.Length - 1]
                               | x -> dna.[x - 1])
    
    //ABCD -> *;A;B;C;D
    let prepareDNA dnaLength (dna:string) : char[] =
        ("*" + dna.PadRight(dnaLength,'*')).ToCharArray()

    //Assumes DNA is all the same length.
    let compare (dna1:char[]) (dna2:char[]) : (char * char)[] =
        dna1 |> Array.zip dna2 //|> Array.map (fun (a,b) -> if a = b && a <> '*' then Some a else None)

    //The state is a tuple which holds (the current substring,  all substrings)
    let extractSubStrings (matches:(char * char)[]) : string list =
         let (currentSubString, allOtherSubstrings) = 
             matches 
             |> Array.fold (fun (currentSubString, allOtherSubstrings) (a,b) -> match ( a = b && a <> '*') with
                                                                                //This isn't a match, so the substring must have ended (or we weren't in one)
                                                                                | false -> ("", if currentSubString = "" then allOtherSubstrings else currentSubString :: allOtherSubstrings)

                                                                                //We have a match, we either start a new substring, or add to the current one
                                                                                | true -> (currentSubString + a.ToString(), allOtherSubstrings)
                                                                            ) ("", [])
        
         if currentSubString = "" then allOtherSubstrings else currentSubString :: allOtherSubstrings |> List.distinct


    // Load in our file
    let fastas = loadFile filename

    // The find the shortest two
    let (shortest, secondShortest, rest) = splitUpFastas fastas

    let dnaLength = secondShortest.Dna.Length
    let a = shortest.Dna |> prepareDNA dnaLength
    let b = secondShortest.Dna |> prepareDNA dnaLength

    let createSubStringsOfLength (substring:char[]) (length:int) : string[] =
        substring |> Array.windowed length |>  Array.map System.String.Concat

    let createSubStrings (substring:string) : string[] =
        let asArray = substring.ToCharArray() 
        [|1..asArray.Length|] |> Array.collect (createSubStringsOfLength asArray)
    

    // The state is a tuple which holds (the current rotated value of b,  all found so far)
    let answer = 
        [1..dnaLength] 
        |> List.fold (fun (dna, foundSoFar) t -> let newSubstrings = compare a dna |> extractSubStrings
                                                 let nextDNA = rotate dna
                                                 let allSubstrings = newSubstrings @ foundSoFar
                                                 (nextDNA,allSubstrings)
                                     ) (b, [""])
        |> snd
        |> List.distinct
        |> Seq.collect createSubStrings
        |> Seq.distinct
        |> Seq.sortByDescending (fun s -> s.Length)
        |> Seq.find (fun s -> rest |> List.exists (fun t -> not (t.Dna.Contains s)) |> not)
    answer

//    // Load in our file
//    let fastas = loadFile filename
//
//    let rec buildUpSubstrings (fastas:Fasta List) prefix : string list =
//        match fastas |> List.tryFind(fun fasta -> not (fasta.Dna.Contains prefix)) with
//        | None -> //All strings contain this prefix
//                  let longerPrefixes =  ["A";"C";"G";"T"] |> List.collect (fun suffix -> (buildUpSubstrings fastas (prefix + suffix) ))
//                  prefix :: longerPrefixes
//        | Some(_) -> //We have a fasta which doesn't contain the prefix
//                  []
//
//    
//    let allSubstrings = [|"A";"C";"G";"T"|] |> Array.Parallel.collect (fun prefix -> (buildUpSubstrings fastas prefix) |> List.toArray)
//    allSubstrings |> Array.sortByDescending (fun d -> d.Length) |> Array.head

//solve "C:\CodeDojo\Rosalind\David_FSharp_Simple\Files\problem14_real.txt" 
solve "C:\CodeDojo\Rosalind\David_FSharp_Simple\Files\problem14_real.txt" = "ACCCGAGACCGGCCCCCAAGTTTGAACGGCCGTGAGCCTAGCCGCAGCGGATTGGAGGCAGGATTGCGCTAGTAGTTACTGGAAGTAATTTGCAGCTCCAGGGTCAAGGAGACACAGCTACGAAATAGTCCCCCTTAGACCAGGCAGCCAACCCGCTAACCCTACGCGCGTTAACTCTTAGATTGAGCTGTTTACTTCCGCTATGTTTCGCTTTTTTAAATTGCCGACGGGCAGCGTAGCCAGTTCAGCGGATACATCACCGGTGCAAAGTACTACCATCACGATTTTTGTT"



(*
    Load file
    Have a list of [A;C;G;T]   

    In parallel
    Is A in all strings,  yes
        A add to results
        Check for AA;AC;AG;AT



*)





//    //Gets the longest of the two strings
//    let longestString (str1:string) (str2:string) : string = 
//        if str1.Length > (str2.Length) then str1 else str2

//    compare a b |> longestSubString

  //  let fastas = loadFile filename

//    //The state is a tuple which holds (the current substring,  the longest substring)
//    let longestSubString (matches:char option[]) : string =
//         matches 
//         |> Array.fold (fun state t -> match t with
//                                       //This isn't a match, so the substring must have ended (or we weren't in one)
//                                       | None -> ("", snd state)
//
//                                       //We have a match, we either start a new substring, or add to the current one
//                                       //checking to see if we have a new longest
//                                       | Some(chr) -> let thisSubString = (fst state) + chr.ToString()
//                                                      let currentBest:string = snd state
//                                                      let newBest = longestString thisSubString currentBest
//                                                      (thisSubString, newBest)
//                                        ) ("", "")
//         |> snd