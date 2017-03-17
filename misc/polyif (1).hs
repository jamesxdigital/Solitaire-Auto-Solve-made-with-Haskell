{-***********************************
	polyif.hs
	Functions which have type variables are said to be polymorphic.
************************************-}
my_min :: Ord a => a -> a -> a
my_min n1 n2 = if n1<n2 then n1 else n2
