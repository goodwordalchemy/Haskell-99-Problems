module PrimeFactors(primeFactors) where

import Prime(isPrime)
import Data.Function(on)


primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n = lpf:(primeFactors . ceiling . (div' n) $ lpf) 
    where lpf = head . (filter (\m -> (n `mod` m == 0))) . (filter isPrime) $ [2..]
          div' = on (/) fromIntegral