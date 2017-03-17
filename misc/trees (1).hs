{-***********************************
trees.hs

In a binary tree, each node holds some data item and has a left-successor and a
right-successor,either or both of which might be empty. A node with no
successors is called a leaf.

                elephant
        bat                 hare
aardvark    cow         fox       jackal
        chicken           goat
************************************-}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)


chars :: Tree String
chars = Node "A"
                (Node "B"
                    (Node "C" Empty Empty)
                    (Node "D" Empty Empty)
                )
                (Node "E"
                    (Node "F" Empty Empty)
                    (Node "G" Empty (Node "H"
                        (Node "I" Empty Empty)
                        Empty
                    ))
                )

animals :: Tree String
animals = Node "Elephant"
              (Node "Bat"
                (Node "Aardvark" Empty Empty)
                (Node "Cow"
                  (Node "Chicken" Empty Empty) Empty)
              )
              (Node "Hare"
                (Node "Fox" Empty
                  (Node "Goat" Empty Empty)
                )
                (Node "Jackal" Empty Empty)
              )

traverseDF :: Tree a -> [a]
traverseDF Empty        = []
traverseDF (Node a l r) = a : (traverseDF l) ++ (traverseDF r)

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node x left_sub_tree right_sub_tree) = (inorder left_sub_tree)++[x]++(inorder right_sub_tree)
