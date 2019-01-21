lsort :: [[a]] -> [[a]]
lsort [] = []
lsort [l] = [l]
lsort (l:ls) = lsort ( filter smaller ls ) ++ (filter same ls) ++ [l] ++ lsort ( filter greater ls )
    where smaller m = ((length m) < length l) 
          greater m = ((length m) > length l) 
          same m = (length m == length l )

length_freq :: [[a]] -> [a] -> Int
length_freq ll l = foldl (\acc x -> acc + check x l) 0 ll
    where check x y = if (length x == length y) then 1 else 0

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort [l] = [l]
lfsort all@(l:ls) = lfsort ( filter smaller ls ) ++ (filter same ls) ++ [l] ++ lfsort ( filter greater ls )
    where smaller m = (length_freq all m) < (length_freq all l)
          greater m = (length_freq all m) > (length_freq all l) 
          same m = (length_freq all m) == (length_freq all l)
    