{-***********************************
	listtypes.hs
	The following are all valid Lists, with their type declarations:
************************************-}
li :: [Int] -- i.e. a List of Ints
li = [1,2,3]

lb :: [Bool]
lb = [True, False, False, True]

lli :: [[Int]] -- a List of Lists of Int
lli = [[1,2,3],[4,5],[6]]

ls :: [String]
ls = ["qwerty"] -- a list of one item

lishort :: [Int]
lishort = [1..100] -- convenient ways of defining list sequences

liadd :: [Int]
liadd =  [2,3,4] ++ [5,6] -- ++ concatenates lists (of the same type)

lilength :: Int
lilength = length lishort --length returns the number of items in a list

linth :: Int
linth = [2,3,4,5]!!2

lihead :: Int
lihead = head lishort --head and tail return the head and tail
