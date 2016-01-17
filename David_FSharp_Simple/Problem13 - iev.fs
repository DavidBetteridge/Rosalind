#if INTERACTIVE
#else
module Problem13
#endif

let solve AA_AA AA_Aa AA_aa Aa_Aa Aa_aa aa_aa = 
//    ((float)AA_AA * 2.0) + ((float)AA_Aa * 2.0) + ((float)AA_aa * 2.0) + ((float)Aa_Aa * 1.5) + ((float)Aa_aa * 1.0) + ((float)aa_aa * 0.0)

    let numberOfOffspring = 2.0
    let weights = [1.0;1.0;1.0;0.75;0.5;0.0]
    let population = [AA_AA;AA_Aa;AA_aa;Aa_Aa;Aa_aa;aa_aa]
    weights |> List.zip population |> List.sumBy (fun (a,b) -> (float)a * b * numberOfOffspring)

solve 19492 19253 19718 19029 16465 16084  = 161934.5
