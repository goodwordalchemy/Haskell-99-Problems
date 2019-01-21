module TruthTable 
( and'
, or'
, nor'
, xor'
, impl'
, equ'
) where

and' :: Bool -> Bool -> Bool
and' a b = a && b

or' :: Bool -> Bool -> Bool
or' a b = a || b

nor' :: Bool -> Bool -> Bool
nor' a b = (not a) && (not b)

xor' :: Bool -> Bool -> Bool
xor' a b = not (a == b)

impl' :: Bool -> Bool -> Bool
impl' a b = if a then b else True

equ' :: Bool -> Bool -> Bool
equ' = (==)

putTableLine :: (Bool -> Bool -> Bool) -> Bool -> Bool -> String
putTableLine func a b = (show a) ++ " " ++ (show b) ++ " " ++ (show $ func a b)


table :: (Bool -> Bool -> Bool) -> IO ()
table func = do
    putStrLn $ putTableLine func True True
    putStrLn $ putTableLine func False True
    putStrLn $ putTableLine func True False
    putStrLn $ putTableLine func False False