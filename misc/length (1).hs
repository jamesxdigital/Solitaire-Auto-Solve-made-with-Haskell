{-***********************************
length.hs
It’s often the case that we want to use a pattern
to decompose a list but we would also like to keep a
handle on the whole of the list

Here @ means ‘bind the variable on the left of
the @ to the value on the right’
************************************-}
my_length :: [a] -> Int
my_length list
  | null list = 0
  | otherwise = 1 + my_length tail
  where (_:tail) = list

my_length_as :: [a] -> Int
my_length_as lis@(_:t)
  | null lis = 0
  | otherwise = 1 + my_length_as t
