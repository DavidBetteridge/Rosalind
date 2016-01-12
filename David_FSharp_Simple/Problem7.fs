module Problem7

let solve k m n = 
    let populationSize = k + m + n
    let totalCombinations =  (2 * populationSize) * (2 * (populationSize - 1) )
    let startWithN = ((2 * n) * (2 * (n - 1))) + (2 * n * m)
    let startWithM = m * ((2 * n) + (m - 1))
    let pR = (decimal)(startWithN + startWithM) / (decimal)totalCombinations
    1m - pR


