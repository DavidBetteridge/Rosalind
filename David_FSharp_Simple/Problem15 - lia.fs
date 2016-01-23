#if INTERACTIVE
#else
module Problem15
#endif

type Outcome =
        {
            Combination : string
            Odds : double
        }

type punnettSquare = string list list

let solve k n = 

    // a A -> Aa
    let (++) (s1:string) (s2:string) = if s1 < s2 then s1+s2 else s2+s1 

    //For example AB x aB -> AaBB 
    let combine (parent1:string) (parent2:string) : string = 
        (parent1.Substring(0,1) ++ parent2.Substring(0,1)) + (parent1.Substring(1,1) ++ parent2.Substring(1,1))

    //For example AB x [AB;Ab;aB;ab] ->
    let createRow (parents:string list) (parent1:string) : string list =
        parents |> List.map (combine parent1)

    //For example AaBb -> ["AB";"Ab";"aB";"ab"]
    let createCombinations (parent:string) : string list =
        [
            parent.Substring(0,1) + parent.Substring(2,1);
            parent.Substring(0,1) + parent.Substring(3,1);
            parent.Substring(1,1) + parent.Substring(2,1);
            parent.Substring(1,1) + parent.Substring(3,1);
        ]
    
    //Given two parents (for example AaBb and AaBB) their punnett square 
    let buildPunnettSquare (parent1:string) (parent2:string) : punnettSquare =
        let columns = createCombinations parent2
        
        parent1 
        |> createCombinations 
        |> List.map (createRow columns)


    //Converts a punnett square into a string for display purposes
    let displayPunnettSquare (grid:punnettSquare) : string = 
        grid 
        |> List.map (String.concat " " )
        |> String.concat "\r\n"
    

    //From a punnett square calculates the odds of them creating the 9 possible outcomes
    let calculateOddsFromPunnettSquare (grid:punnettSquare) : Outcome list = 
        let combinations = grid |> List.collect id
        let outcomes = ["AABB";"AaBB";"aaBB";"AABb";"AaBb";"aaBb";"AAbb";"Aabb";"aabb"]
        outcomes 
        |> List.map (fun oc ->  {Outcome.Combination=oc; Outcome.Odds = (combinations 
                                                                         |> List.where (fun c -> c = oc) 
                                                                         |> List.length 
                                                                         |> double) / 16.0 })

        
    let gen1 = buildPunnettSquare "AaBb" "AaBb" |> displayPunnettSquare

    let gen1 = buildPunnettSquare "AaBb" "AaBb" |> calculateOddsFromPunnettSquare


    let gen2 = gen1 
               |> List.map (fun oddsFromGen1 -> oddsFromGen1, buildPunnettSquare "AaBb" oddsFromGen1.Combination |> List.filter (fun g -> g.Combination="AaBb" ))
               |> List.map (fun (od, g) -> g |> List.sumBy(fun o -> od.Odds * o.Odds))
               |> List.sum
    gen2

//    let buildParentFromString (text:string) = 
//        ((text.Chars(0) = 'A' || text.Chars(0) = 'B'), (text.Chars(1) = 'A' || text.Chars(1) = 'B'))
//
//    //For example Aa Aa --> AA, Aa aa
//    let odds (parent1:parent) (parent2:parent) =
//        match parent1, parent2 with
//        | (true,true), (true,true) -> 1.0, 0.0, 0.0
//        | (true,true), (true,false) -> 0.5, 0.25, 0.0
//        | (true,true), (false,true) -> 0.75, 0.25, 0.0
//        | (true,true), (false,false) -> 0.0, 1.0, 0.0
//        | (true,false), (true,true) -> 0.5, 0.5, 0.0
//        | (true,false), (true,false) -> 0.25, 0.5, 0.25
//        | (true,false), (false,true) -> 0.25, 0.5, 0.25
//        | (true,false), (false,false) -> 0.0, 0.5, 0.5
//        | (false,true), (true,true) -> 0.5, 0.5, 0.0
//        | (false,true), (true,false) -> 0.25, 0.5, 0.25
//        | (false,true), (false,true) -> 0.25,0.5, 0.25
//        | (false,true), (false,false) -> 0.0,0.5,0.5
//        | (false,false), (true,true) -> 0.0,1.0,0.0
//        | (false,false), (true,false) -> 0.0, 0.25, 0.75
//        | (false,false), (false,true) -> 0.0, 0.25, 0.75
//        | (false,false), (false,false) -> 0.0,0.0,1.0
//
//    
//    let oddsForAa (parent1:parent) (parent2:parent) =
//     match parent1, parent2 with
//        | (true,true), (true,true) -> 0.0
//        | (true,true), (true,false) -> 0.5, 0.25, 0.0
//        | (true,true), (false,true) -> 0.75, 0.25, 0.0
//        | (true,true), (false,false) -> 0.0, 1.0, 0.0
//        | (true,false), (true,true) -> 0.5, 0.5, 0.0
//        | (true,false), (true,false) -> 0.25, 0.5, 0.25
//        | (true,false), (false,true) -> 0.25, 0.5, 0.25
//        | (true,false), (false,false) -> 0.0, 0.5, 0.5
//        | (false,true), (true,true) -> 0.5, 0.5, 0.0
//        | (false,true), (true,false) -> 0.25, 0.5, 0.25
//        | (false,true), (false,true) -> 0.25,0.5, 0.25
//        | (false,true), (false,false) -> 0.0,0.5,0.5
//        | (false,false), (true,true) -> 0.0,1.0,0.0
//        | (false,false), (true,false) -> 0.0, 0.25, 0.75
//        | (false,false), (false,true) -> 0.0, 0.25, 0.75
//        | (false,false), (false,false) -> 0.0,0.0,1.0
//
//    let firstGen (parent1:string) = 
//        let parent1A = parent1.Substring(0, 2) |> buildParentFromString
//        let parent2A = "Aa" |> buildParentFromString
//        let (AA,Aa,aa) = odds parent1A parent2A
//        (AA,Aa,aa)
//
//
//    firstGen "AaBa" 
//
//    let AA = 0.25
//    let Aa = 0.5
//    let aa = 0.25

    //1.0


//solve 2 1 //= 0.684



