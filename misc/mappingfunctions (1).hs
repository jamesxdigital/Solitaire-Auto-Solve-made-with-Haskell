{-***********************************
mappingfunctions.hs

• if you want to perform the same operation on each member of a list, use map.
• if you want to perform a test on each member of a list, use filter.
• if you want to process a list and return a single result, use foldr.
************************************-}
{-***********************
--import libraries
*************************-}
import Debug.Trace

{-***********************
--exam mark analysis
*************************-}
type Mark_list = [(String,Int)] --define type Mark_list

below_average :: Mark_list->(Float,[String]) --takes list of marks, returns average and list underaverage
below_average mrklist =
    let marks = map snd mrklist --use snd mrklist --  map snd markA -->> [55,67,37,32,100]
        average = (fromIntegral (foldr (+) 0 marks))/(fromIntegral (length mrklist)) -- use foldr to add the marks up, divided by length
        belowlist = filter (\e -> (fromIntegral (snd e)) < average) mrklist --filter the below-av items from mrklist
    in (average,map fst belowlist) --map with fst to get the names below av

markA :: Mark_list --example mark list, markA
markA = [("able",55),("baker",67),("charles",37), ("dogbreath",32),("james",100)]


{-***********************
--merge (!order)
*************************-}

my_merge :: Ord a => (a->a->Bool) -> [a] -> [a] -> [a] --takes two lists and returns a merged list of the two
my_merge compfunc [] list2 = list2 --list1 is empty, return list2
my_merge compfunc list1 [] = list1 --list2 is empty, return list1
my_merge compfunc (h1:t1) (h2:t2) --compfunc returns true or false
  | compfunc h1 h2 = (h1:my_merge compfunc t1 (h2:t2)) --if h1>h2, put h1 to head the recurse through rest of list
  | otherwise      = (h2:my_merge compfunc t2 (h1:t1))
-- my_merge (>) [5,4] [3,2,1] ~> [5,4,3,2,1]
-- my_merge (<) [1,2,3] [4,5] ~>  [1,2,3,4,5]
-- my_merge (\str1 str2 -> (length str1) < (length str2)) ["qw","erty"] ["q","wer","qwert"] ~> [“q”, “qw”, “wer”, “erty”, “qwert”]

{-***********************
--mergesort (order)
*************************-}
mergesort :: (Ord a, Show a) => (a->a->Bool) -> [a] -> [a] --takes in one unordered list and returns an ordered list
mergesort compfunc [] = [] --check this only once
mergesort compfunc dlist =
  (mergesortA compfunc (map (\e -> [e]) dlist)) --take items in list and create sublists. [5, 3, 6] -->> [[5], [3], [6]]

mergesortA :: (Ord a, Show a) => (a->a->Bool) -> [[a]] -> [a] --mergesortA will return a list of length 1, like [[1,2,3,4]]
mergesortA _ [list] = list
mergesortA compfunc mlist = mergesortA compfunc (mergesortpass compfunc mlist) --by recursive calls it transforms sublists of length 1 to sublists of length 2, then 4 ..

mergesortpass :: Ord a => (a->a->Bool)->[[a]] -> [[a]] --uses my_merge
mergesortpass _ [] = []
mergesortpass _ [l] = [l] --one element, return list unchanged
mergesortpass compfunc (list1:(list2:rest)) = (my_merge compfunc list1 list2): mergesortpass compfunc rest --merge first two losts
