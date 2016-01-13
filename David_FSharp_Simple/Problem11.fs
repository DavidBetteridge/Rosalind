#if INTERACTIVE
#else
module Problem11
#endif

let solve n m = 
    
    let nextMonth (population:bigint[]) =
        Array.init m (function
                      | 0 -> (population |> Array.skip 1 |> Array.sum)
                      | x -> population.[x - 1])

    let initialPopulation = (Array.init m (fun i -> if i = 0 then bigint(1) else bigint.Zero))
    
    [1..(n - 1)] 
        |> List.fold (fun population _ -> nextMonth population) initialPopulation
        |> Array.sum

//    let rec inner (currentPopulation:bigint[]) (monthNumber:int) =
//        match monthNumber = n with
//        | true -> currentPopulation |> Array.sum
//        | false -> inner (nextMonth currentPopulation) (monthNumber + 1)
//
//    inner (Array.init m (fun i -> if i = 0 then bigint(1) else bigint.Zero)) 1


    
solve 96 16 = bigint.Parse("50828740550995771442")
//solve 6 3 = 4