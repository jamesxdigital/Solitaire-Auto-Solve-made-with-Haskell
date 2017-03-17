module Random where

  import System.Random
  rlis = take 3 (randoms (mkStdGen 42):: [Int])
  zlis = zip [("ONE","TWO"),("TWO","FOUR"),("FOUR","EIGHT")] rlis
  slis = mergesort (\(_,n1) (_,n2)->n1<n2) zlis
  shuffle = map fst slis

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

  mergesortA :: (Ord a) => (a->a->Bool) -> [[a]] -> [a] --mergesortA will return a list of length 1, like [[1,2,3,4]]
  mergesortA _ [list] = list
  mergesortA compfunc mlist =  mergesortA compfunc (mergesortpass compfunc mlist) --by recursive calls it transforms sublists of length 1 to sublists of length 2, then 4 ..

  mergesortpass :: Ord a => (a->a->Bool)->[[a]] -> [[a]] --uses my_merge
  mergesortpass _ [] = []
  mergesortpass _ [l] = [l] --one element, return list unchanged
  mergesortpass compfunc (list1:(list2:rest)) = (my_merge compfunc list1 list2): mergesortpass compfunc rest --merge first two losts
