import Data.Function(on)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)


data Choice = TLeft | TRight deriving (Eq, Show)

type Path = [Choice]

node :: Tree Char
node = Branch 'x' Empty Empty

choiceCombs :: Int -> [[Choice]]
choiceCombs 0 = [[]]
choiceCombs n = [TLeft:l | l <- bc] ++ [TRight:l | l <- bc]
    where bc =  choiceCombs (n-1)


insertNodeForChoice :: Tree Char -> [Choice] -> Tree Char
insertNodeForChoice _ [choice]
    | choice == TLeft = Branch 'x' node Empty
    | otherwise =  Branch 'x' Empty node
insertNodeForChoice (Branch _ left right) (TLeft:cs) = Branch 'x' (insertNodeForChoice left cs) right
insertNodeForChoice (Branch _ left right) (TRight:cs) = Branch 'x' left (insertNodeForChoice right cs)

nearestCompleteTreeDepth :: Integer -> Integer
nearestCompleteTreeDepth n = fromIntegral $ floor ((logBase `on` fromIntegral) 2 (n+1))

sizeOfCompleteTree :: Integer -> Integer
sizeOfCompleteTree n = 2 ** (nearestCompleteTreeDepth n) - 1


genenerateCompleteTree :: Integer -> Tree Char
genenerateCompleteTree 0 = Empty
genenerateCompleteTree 1 = node
genenerateCompleteTree n = Branch 'x' (genenerateCompleteTree (n-1)) (genenerateCompleteTree (n-1))


combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = []
combinations 1 l = [[x] | x <- l]
combinations n (x:xs) = [x:ys | ys <- combinations (n-1) xs] ++ [ys | ys <- combinations n xs] 

pathCombinations :: Integer -> [Path]-> [[Path]]
pathCombinations n paths = 


-- cbalTree :: Int -> [Tree]
-- cbalTree 0 = Empty
-- cbalTree 1 = Branch 'x' Empty Empty
-- cbalTree 2 = [ Branch 'x' (cbalTree 1) Emtpy
--              , Branch 'x' Emtpy, (cbalTree 1)
--              ]


tree = Branch 'x' (Branch 'x' (Branch 's' Empty Empty) Empty) (Branch 'x' Empty Empty) 
