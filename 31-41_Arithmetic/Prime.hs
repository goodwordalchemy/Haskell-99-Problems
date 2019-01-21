module Prime(isPrime) where

helper :: [Integer] -> Integer -> Bool
helper (x:xs) p 
    | x < p = helper (filter (\y -> (y `mod` x) /= 0 ) xs) p
    | x == p = True
    | x > p = False


isPrime :: Integer -> Bool
isPrime p = helper [2..] p