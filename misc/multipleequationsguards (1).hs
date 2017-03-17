{-***********************************
	multipleequationsguards.hs
  use several equations, where each equation triggers on a different parameter pattern
  Using guards instead of if's
************************************-}

--member
member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (h:t)
  | x==h = True
  | otherwise = member x t

--identical
identical :: Eq a => [a] ->[a] -> Bool
identical [] [] = True
identical [] _  = False --if this list is empty, the other doesnt matter
identical _ [] = False
identical (h1:t1) (h2:t2) --header1:tail1--header2:tail2
  | h1==h2 = identical t1 t2
  | otherwise = False

--length
my_length :: [a] -> Int
my_length lis
  | null lis = 0
  | otherwise = 1 + my_length t
                    where (_:t) = lis
