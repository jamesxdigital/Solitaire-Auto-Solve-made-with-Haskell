{-***********************************
highordfunc.hs

• If you want to perform the same operation on each member of a list, use map.
• If you want to perform a test on each member of a list, use filter.
• If you want to process a list and return a single result, use foldr.
************************************-}

--map (take a list and perform the same operation on each of its members)
my_map :: (a->b) -> [a] -> [b]
my_map _ [] = []
my_map f (h:t) = (f h : my_map f t)
-- my_map sqrt [9.0,16.0,25.0] ~> [3.0,4.0,5.0]
-- my_map length [[1,2,3],[4,5],[6,7,8,9]] ~> [3,2,4]

--anonymous (if we want to add a constant, 5, to each item in an integer list)
add5 :: Int -> Int
add5 n = n+5
--my_map add5 [1,2,3] ~> [6,7,8]
--can be written as --->>> my_map ( \n -> n + 5)  [1,2,3] ~> [6,7,8]

{-***********************************-}

--filter (perform a test on each item in a list, return only items which pass test)
my_filter :: (a->Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter f (h:t)
    |f h = (h:my_filter f t)
    |otherwise = my_filter f t
-- filter (\n -> (n > 100)) [87, 112, 54, 225] ~> [112,225]
-- filter (\l->(length l)==2) [[1,2], [3,4,5], [6,7], [8]] ~> [[1,2],[6,7]]

{-***********************************-}

--foldr (take a list and produce a single result)
my_foldr :: (a->b->b)->b->[a] -> b --b(base) [a]list
my_foldr _ base [] = base
my_foldr f base (h:t) = f h (my_foldr f base t) --f(function)
--apply the function f to the first in the list and the result of folding the rest of the lis
--foldr (+) 0 [1,2,3,4] ~> 10