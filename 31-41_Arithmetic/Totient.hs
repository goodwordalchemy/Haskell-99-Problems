import Coprime(isCoprime) 

totient :: Integer -> Integer
totient n = fromIntegral . length . (filter (isCoprime n)) $ [1..n-1]