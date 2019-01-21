module Euclid(gcd') where

gcd' :: Integer -> Integer -> Integer
gcd' a b 
    | b == 0 = a
    | otherwise = gcd' b $ a `mod` b