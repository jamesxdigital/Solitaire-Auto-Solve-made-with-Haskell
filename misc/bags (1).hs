{-***********************************
bags.hs
Author: James Milton
Date last modified: 22/10/15
************************************-}


module Bags where

  import Data.List
  --polymorphic type synonym which displays a data type and it's quantity inside a tuple
  type Bag a = [(a,Int)]

  --bagInsert
  bagInsert :: Eq a => a -> Bag a -> Bag a --Eq type class to support == operator
  bagInsert itemAdd bag
        |null bag = [(itemAdd,1)] --item to add is not within bag, add it to bag
        |itemAdd==item = ((itemAdd,val+1): restbag)--item to add is already in bag, add amount by 1
        |otherwise = ((item,val):bagInsert itemAdd restbag)--perform bagInsert on rest of bag
        where ((item,val):restbag) =bag --where (item,val) is the head and restbag is the tail

  --listToBag
  listToBag :: Eq a => [a] -> Bag a --Eq type class needed for use of bagInsert
  listToBag list
        |null list = [] --no items in list, return empty bag
        |otherwise= bagInsert h (listToBag t) --uses bagInsert to add head to Bag
        where (h:t) = list

  --listToBagGroup
  listToBagGroup :: Ord a => [a] -> Bag a
  listToBagGroup list = --listToBagGroup [1,2,1,3,2,3]
                        map (\p->(head p, length p)) --[(1,2),(2,2),(3,2)]
                                                    (group --["11","22","33"]
                                                          (sort list)) --"1,1,2,2,3,3"

  --bagEqual
  bagEqual :: Eq a => Bag a -> Bag a -> Bool --Eq type class needed to support /= operator
  bagEqual [][] = True --both bags empty, return equal
  bagEqual bagA bagB
      | elem (item1,val1) bagB = bagEqual restBagA (bagDeleteItem item1 bagB) --the head of bagA is in BagB, perform bagEqual
      | otherwise = False                                                     -- on rest of bagA and bagB excluding that item
      where ((item1,val1):restBagA) = bagA
            ((item2,val2):restBagB) = bagB

  --bagSum
  bagSum :: Eq a => Bag a -> Bag a -> Bag a --Eq type class needed for use of bagContains
  bagSum [][] =[] --both bags empty, return equal
  bagSum bagA bagB
      |null bagA = bagB --no items in bagA, return bagB
      |bagContains item1 bagB = (item1,(val1+(bagItemQuantity item1 bagB))): --item in head of bagA is in bagB, add quantity of item in
            (bagSum restBagA (bagDeleteItem item1 bagB))--bagB to the quantity of the head of bagA. Then recurse through rest of bagA
      |otherwise = ((item1,val1):bagSum bagB restBagA) --head of item not in bag b, add it to head of bag and recurse through rest of bagA
      where ((item1,val1):restBagA) = bagA
            ((item2,val2):restBagB) = bagB

  --bagIntersection
  bagIntersection :: Eq a => Bag a -> Bag a -> Bag a --Eq type class needed for use of bagContains
  bagIntersection [][] = [] --given two empty bags, intersection is an empty bag
  bagIntersection bagA bagB
      | null bagA = []
      | bagContains item1 bagB = (item1,(minIntValue val1 (bagItemQuantity item1 bagB))): --item1 is in bagB, then
            (bagIntersection restBagA bagB)             --then find the min value in both bags then perform on rest of bagA
      | otherwise = bagIntersection bagB restBagA --recerse through bagB and rest of bagA
      where ((item1,val1):restBagA) = bagA
            ((item2,val2):restBagB) = bagB

  --Support Functions
  --bagContains
  bagContains :: Eq a => a -> Bag a -> Bool --returns boolean depending if item is within given bag
  bagContains x [] = False --bag is empty, always return false
  bagContains x bag
      | x==item = True --item desired is in head, return true
      | otherwise = bagContains x restbag --recurse through rest of bag
      where ((item,val):restbag) =bag

  --bagItemQuantity
  bagItemQuantity :: Eq a => a -> Bag a -> Int --given an item and a bag, return quantity of item in bag
  bagItemQuantity item bagA
      | null bagA = 0 --item not in bag, return 0
      | item==itemA = valA --item desired equals item in head, return value
      | otherwise = bagItemQuantity item restbagA --recruse through rest of bag
      where ((itemA,valA):restbagA) = bagA

  --bagDelete
  bagDeleteItem :: Eq a => a -> Bag a -> Bag a --given an item and a bag, returns bag without that item
  bagDeleteItem _ [] = [] --bag empty, will always return empty bag
  bagDeleteItem toDelete bag
      |null bag = error "Item is not in bag" --item not in bag, throw error
      |item==toDelete = restbag --item to delete is item in head of bag, return rest of bag
      |otherwise = ((item,val):bagDeleteItem toDelete restbag) --otherwise, recurse through rest of bag with rest of bag
      where ((item,val):restbag) = bag

  --minIntValue
  minIntValue :: Int -> Int -> Int --given two ints, will return smallest int
  minIntValue intA intB
      | intA <= intB = intA --intA smaller than intB, return intA
      | otherwise = intB --otherwise return intB

  --pre-defined bags for testing
  bagA :: Bag String -- a is String
  bagA = [("A",3),("B",4),("C",2)]
  bagB :: Bag Int -- a is String
  bagB = [(123,3),(456,4),(789,2)]
  bagC :: Bag String -- a is String
  bagC = [("A",3),("B",4),("C",2),("D",1)]
  bagD :: Bag String -- a is String
  bagD = [("B",4),("A",3),("C",2)]
