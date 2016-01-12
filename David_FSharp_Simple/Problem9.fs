module Problem9

let solve (s:string) (t:string) : string = 
    [0..(s.Length - t.Length)]
    |> List.filter (fun i -> s.[i..(i + t.Length - 1)] = t)
    |> List.map (fun i -> (i + 1).ToString())
    |> String.concat " "

//solve "GATATATGCATATACTT" "ATAT" = "2 4 10"


//let j = s.ToCharArray()
//        |> Array.windowed (t.Length)
//        |> Array.mapi (fun i b -> i, b |> System.String.Concat)
//        |> Array.filter (fun (i, b) -> b = t)
//        |> Array.map (fun (i, b) -> i + 1)