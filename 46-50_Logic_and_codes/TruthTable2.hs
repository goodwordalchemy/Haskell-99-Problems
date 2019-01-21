import Control.Monad(mapM_)
import Data.List(intersperse)

import TruthTable

putTableLine :: ([Bool] -> Bool) -> [Bool] -> String
putTableLine func l = concat ( intersperse " " (map show (l ++ [func l]) ))

boolCombs :: Int -> [[Bool]]
boolCombs 0 = [[]]
boolCombs n = [True:l | l <- bc] ++ [False:l | l <- bc]
    where bc =  boolCombs (n-1)


table :: Int -> ([Bool] -> Bool) -> IO ()
table n func = mapM_ (putStrLn . putTableLine func) (boolCombs n)