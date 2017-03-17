{-***********************************
	maybe.hs

  The polymorphic enumerated type Maybe provides a neat way of dealing with error
  conditions:

  We can use Maybe in cases where a function might not return its usual value
  because of an error condition:

  data Maybe a = Nothing | Just a
                deriving (Eq,Ord,Read,Show) --this type should inherit from the Eq, Ord, Read and Show type classes
************************************-}

my_nth :: Int -> [a] -> Maybe a
my_nth n lis
  |length lis < n = Nothing
  |otherwise = Just (my_nthA n lis)

my_nthA :: Int -> [a] -> a
my_nthA 0 (first : _) = first
my_nthA n (_ : rest) = my_nthA (n-1) rest

--my_nth 5 [1,2,3,4] ~> Nothing
--my_nth 2 [1,2,3,4] ~> Just 3
