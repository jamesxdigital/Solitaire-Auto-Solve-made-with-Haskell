{-***********************************
	removefunction.hs
  remove removes all occurrences of a given item in a given
  list, e.g. remove 'q' ['a','q','b','q','c'] ~> [‘a’,’b’,’c’]
************************************-}
remove :: Eq a => a->[a] -> [a]
remove _ [] = [] --will return empty list regardless of first item
remove x (h:t)
  | x == h = remove x t
  | otherwise = h : remove x t --removing x from the remainder of the List, t.
