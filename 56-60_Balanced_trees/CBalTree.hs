import Data.Function(on)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

data Choice = TLeft | TRight deriving (Eq, Show)

type Path = [Choice]


node :: Tree Char
node = Branch 'x' Empty Empty


choiceCombs :: Integer -> [[Choice]]
choiceCombs 0 = [[]]
choiceCombs n = [TLeft:l | l <- bc] ++ [TRight:l | l <- bc]
    where bc =  choiceCombs (n-1)


insertNodeForChoice :: Tree Char -> [Choice] -> Tree Char
insertNodeForChoice _ [] = Empty 
insertNodeForChoice _ [choice]
    | choice == TLeft = Branch 'x' node Empty
    | otherwise =  Branch 'x' Empty node
insertNodeForChoice (Branch _ left right) (TLeft:cs) = Branch 'x' (insertNodeForChoice left cs) right
insertNodeForChoice (Branch _ left right) (TRight:cs) = Branch 'x' left (insertNodeForChoice right cs)


nearestCompleteTreeDepth :: Integer -> Integer
nearestCompleteTreeDepth n = fromIntegral $ floor ((logBase `on` fromIntegral) 2 (n+1))


nearestCompleteTreeSize :: Integer -> Integer
nearestCompleteTreeSize n = sum $ map twoToThe [0..depth-1]
    where 
        twoToThe l = toInteger . floor $ 2**(fromIntegral l)
        depth = nearestCompleteTreeDepth n


generateCompleteTree :: Integer -> Tree Char
generateCompleteTree 0 = Empty
generateCompleteTree 1 = node
generateCompleteTree depth =  Branch 'x' (generateCompleteTree (depth-1)) (generateCompleteTree (depth-1))


combinations :: (Eq a) => Integer -> [a] -> [[a]]
combinations _ [] = []
combinations 0 _ = []
combinations 1 l = [[x] | x <- l]
combinations n (x:xs) = [x:ys | ys <- combinations (n-1) xs] ++ [ys | ys <- combinations n xs] 


pathCombinations :: Integer -> [Path]-> [[Path]]
pathCombinations n paths = combinations numDangleNodes pathCombosAtDepth
    where numDangleNodes = n - nearestCompleteTreeSize n
          pathCombosAtDepth = choiceCombs $ (nearestCompleteTreeDepth n)

cBalTreeFromPaths :: Tree Char -> [Path] -> Tree Char
cBalTreeFromPaths tree [] = tree
cBalTreeFromPaths tree [p] = insertNodeForChoice tree p
cBalTreeFromPaths tree (x:xs) = cBalTreeFromPaths newTree xs
    where newTree = insertNodeForChoice tree x


cbalTree :: Integer -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n 
    | (length paths) == 0 = [completeTree]
    | otherwise  = map (\p -> cBalTreeFromPaths completeTree p) paths
        where completeTree = generateCompleteTree completeTreeDepth
              paths = pathCombinations n allPathsOfN
              allPathsOfN = choiceCombs completeTreeDepth
              completeTreeDepth = nearestCompleteTreeDepth n
