import DiffSelect (diff_select)

randomPermutation :: [a] -> IO [a]
randomPermutation [] = return []
randomPermutation l = do 
    daList <- diff_select (length l) (length l)
    return $ map (\n -> l!!(n-1)) daList