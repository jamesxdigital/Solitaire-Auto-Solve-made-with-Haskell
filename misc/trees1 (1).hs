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

data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)

animals :: Tree String
animals =  Node "elephant"
                  (Node "bat"
                        (Leaf "aardvark")
                        (Node "cow"
                              (Leaf "chicken")
                              Empty))
                  (Node "hare"
                        (Node "fox"
                              Empty
                              (Leaf "goat"))
                        (Leaf "jackal"))
--Tree Traversal
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Leaf x) = [x] --leaf returns list of 1 item
inorder (Node x left_sub_tree right_sub_tree) =
          (inorder left_sub_tree)++ --visit left sub tree
          [x]++                     --visit root
          (inorder right_sub_tree)  --visit right sub tree

--Searching a Tree
tree_member :: Ord a => a -> (Tree a) -> Bool
tree_member _ Empty = False
tree_member y (Leaf x) = (x==y) --looking for y in a tree with leaf x
tree_member y (Node x left_sub_tree right_sub_tree)
            | x==y = True --if x and y are the same
            | x>y =  tree_member y left_sub_tree
            | otherwise = tree_member y right_sub_tree
              -- if x>y, y must be in left_sub_tree otherwise
              --look in right sub tree

--Tree Insertion
tree_insert :: Ord a => a-> (Tree a) -> Tree a
tree_insert y Empty = Leaf y --insert into an Empty tree produces a Leaf
tree_insert y (Leaf x) --insert into a Leaf produces a Node
  | x==y = (Leaf x) --already there - leave as it is
  | y<x = (Node x (Leaf y) Empty) --y in left_sub_tree
  | otherwise = (Node x Empty (Leaf y)) --in right_sub_tree
tree_insert y tree --insert into a Node - insert in left or right sub_tree
  | x==y = (Node x left_sub_tree right_sub_tree)
  | y<x  = (Node x (tree_insert y left_sub_tree) right_sub_tree)
  |otherwise =(Node x left_sub_tree (tree_insert y right_sub_tree))
  where (Node x left_sub_tree right_sub_tree) = tree
  -- inorder (tree_insert "finch" animals) ->
  --["aardvark","bat","chicken","cow","elephant","finch","fox","goat","hare","jackal"]
