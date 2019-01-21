module DiffSelect (diff_select) where

import System.Random

diff_select :: Int -> Int -> IO [Int]
diff_select k m = do
    gen <- getStdGen
    return $ helper k m gen []

helper :: Int -> Int -> StdGen -> [Int] -> [Int]
helper 0 m gen l = l
helper k m gen l
    | rand_num `elem` l = helper k m new_gen l 
    | otherwise = helper (k-1) m new_gen (rand_num:l) 
    where (rand_num, new_gen) = randomR (1,m) gen 

main = do
    result <- diff_select 10 10
    putStrLn $ show result
