import Combinations(combinations)

type Persons = (String, String,String,String,String,String,String,String,String )

to_list :: Persons -> [String]
to_list (a0, a1, a2, a3, a4, a5, a6, a7, a8)  = [a0, a1, a2, a3, a4, a5, a6, a7, a8] 

-- group3 :: Persons -> [[[String]]]
-- group3 p = foldl (\acc aList -> (acc ++ (buildComp aList))) [] ( combinations 4 pList )
--     where pList = to_list p
--           buildComp l = [[l
--                          , x
--                          , (setMinusL (setMinusL pList l) x)
--                          ] | x <- combinations 3 (setMinusL pList l)
--                         ]
--           setMinusL l1 l2 = filter (\elt -> not (elt `elem` l2)) l1


group3' :: Persons -> (Int, Int, Int) -> [[[String]]]
group3' p (a, b, c) = foldl (\acc aList -> (acc ++ (buildComp aList))) [] ( combinations a pList )
    where pList = to_list p
          buildComp l = [[l
                        , x
                        , (setMinusL (setMinusL pList l) x)
                        ] | x <- combinations b (setMinusL pList l)
                      ]
          setMinusL l1 l2 = filter (\elt -> not (elt `elem` l2)) l1