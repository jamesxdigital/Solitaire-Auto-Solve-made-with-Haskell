{-***********************************
	intersectionfunction.hs
	This is the intersection of two Lists
	representing sets, i.e. with no repeated
	elements: intersection [1,2,3] [2,3,4,5] ~> [2,3]
************************************-}
intersection :: Eq a => [a]->[a]->[a]

intersection [] _ = []
intersection _ [] = []

intersection (h1:t1) lis2
	| elem h1 lis2 = h1:intersection t1 lis2
	| otherwise = intersection t1 lis2
