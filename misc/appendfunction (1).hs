{-***********************************
	appendfunction.hs
************************************-}
append :: [a]->[a] -> [a]
append [] lis2 = lis2
append (f:r) lis2 = (f:append r lis2)
