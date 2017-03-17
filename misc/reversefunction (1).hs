{-***********************************
	reverse.hs
  reverses the elements of a List i.e. reverse [1,2,3,4] ~> [4,3,2,1]
************************************-}

my_reverse :: [a] -> [a]
my_reverse lis = my_reverseA lis []

my_reverseA :: [a]->[a] -> [a]
my_reverseA [] out = out
my_reverseA (first:rest) out =
  my_reverseA rest (first:out)

--alternative using concatenation operator ++
rev :: [a] -> [a]
rev [] = []
rev (h:t) = rev t ++ [h]
