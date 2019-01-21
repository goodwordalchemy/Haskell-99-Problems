import PrimeFactors(primeFactors)

primeFactorsMult :: Integer -> [(Integer, Integer)]
primeFactorsMult n = foldl incOrAppend [(x, 1)] xs
    where
        (x:xs) = primeFactors n
        incOrAppend all@((num, count):rest) y 
            | y == num = ((num, count + 1):rest)
            | otherwise = ((y, 1): all)