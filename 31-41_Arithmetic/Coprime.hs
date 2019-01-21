module Coprime(isCoprime) where

import Euclid(gcd')

isCoprime :: Integer -> Integer -> Bool
isCoprime n m = (gcd' m n == 1) 