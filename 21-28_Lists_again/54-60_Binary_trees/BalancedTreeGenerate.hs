data Tree a = Empty | Branch a (Tree a) (Tree a)

cbalTree :: Int -> [Tree]
cbalTree 0 = Empty
cbalTree 1 = Branch 'x' Empty Empty
cbalTree 2 = [ Branch 'x' (cbalTree 1) Emtpy
			 , Branch 'x' Emtpy, (cbalTree 1)
			 ]


type Choice = Left | Right


choiceCombs :: Int -> [[Choice]]
choiceCombs 0 = [[]]
choiceCombs n = [Left:l | l <- bc] ++ [Right:l | l <- bc]
    where bc =  choiceCombs (n-1)


type Node = Branch 'a' Empty Empty

insertNodeForChoice :: Node -> [Choice] -> Node
insertNodeForChoice _ [choice] = if choice == Left then 'x' (Branch 'x' Empty) else 'x' (Branch Empty 'x')  
insertNodeForChoice (Branch _ left right) (Left:cs) = 'x' (insertNodeForChoice left cs) right
insertNodeForChoice (Branch _ left right) (Left:cs) = 'x' left (insertNodeForChoice right cs)

tree = Branch "x" (Branch "x" (Branch "s" Empty Empty) Empty) (Branch "x" Empty Empty) 