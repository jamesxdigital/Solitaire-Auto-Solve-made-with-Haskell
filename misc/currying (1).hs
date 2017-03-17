{-***********************************
currying.hs
************************************-}

multiply :: Int -> Int -> Int
multiply x y = x*y

times6 = multiply 6
--times6 7 ~> 42

{-***********************************
   map ( \n -> n + 5)  [1,2,3] ~> [6,7,8]
But we can equally say
   map (5+) [1,2,3] ~> [6,7,8]
again,
   filter (\n -> (n > 100)) [87, 112, 54, 225] ~> [112,225]
can be written
   filter (<100) [87, 112, 54, 225] ~> [112,225]
************************************-}

countElems :: Eq a => a -> [a] -> Int
countElems x list = length (filter (==x) list)

count6 = countElems 6
--count6 [3,2,6,5,6,7] ~> 2
