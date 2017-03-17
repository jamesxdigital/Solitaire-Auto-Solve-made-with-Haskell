{-***********************************
	multipleequations.hs
  use several equations, where each equation triggers on a different parameter pattern
************************************-}

-- member (checks if item is in list)
member :: Eq a => a -> [a] -> Bool
member x  [] = False
member x  (h:t) = if (x==h) then True else (member x t) --combines header and tail for us

--identical (checks to see if to lists are identical)
identical :: Eq a =>[a]->[a]->Bool
identical [] [] = True
identical [] _  = False --if this list is empty, the other doesnt matter
identical _ [] = False
identical (h1:t1) (h2:t2) = --header1:tail1--header2:tail2
                            -- operator : is a constructor for lists. Any list can be constructed using cons: 1:2:3:[]~>[1,2,3]
  if h1==h2
    then (identical t1 t2)
    else False

-- length (finds the length of a list)
my_length :: [a] -> Int
my_length  [] = 0
my_length  (_:rest) = 1 + my_length rest
