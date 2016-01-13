// Add the FParsec NUGET package

#if INTERACTIVE
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsecCS.dll"
#r "C:\\Compiler\\LogoTest\\packages\\FParsec.1.0.2\\lib\\net40-client\\FParsec.dll"
#else
module Problem5
#endif

let solve (filename:string) : string =
    let result =
        loadFile filename
            |> List.map (fun fasta -> fasta, calculateGCContent fasta.Dna )
            |> List.maxBy (fun (_, calc) -> calc)
     
    sprintf "%s\n%f" (fst result).Label (snd result)

solve "C:\CodeDojo\Rosalind\Problem 5\problem_5_dataset.txt"
