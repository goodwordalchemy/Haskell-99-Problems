module Combinations (combinations) where

permutations :: (Eq a) => Int -> [a] -> [[a]]
permutations _ [] = []
permutations 0 _ = []
permutations 1 l = [[x] | x <- l]
permutations n l = [x:m | x <- l, m <- permutations (n-1) l, not (x `elem` m)]



combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = []
combinations 1 l = [[x] | x <- l]
combinations n (x:xs) = [x:ys | ys <- combinations (n-1) xs] ++ [ys | ys <- combinations n xs] 

