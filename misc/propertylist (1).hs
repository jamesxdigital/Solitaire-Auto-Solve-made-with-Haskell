{-***********************************
	propertylist.hs
  propertylist to handle the following functions:

  pcreate returns a new, empty plist

􏰀  pput adds a prop, and its value, to an existing plist, returning the new plist.

􏰀  pdelete deletes a prop and its value from a plist and returns the resulting plist.

  pget retrieves data from plists: i.e. given a prop and a plist, it returns the
  corresponding value (if the prop is present: otherwise we should return an
  indication that the search failed).



  EXAMPLE:
  pput "email" "james.milton@me.com" plis1
  pget "address" plis1
  pdelete "address" plis1
************************************-}
module Proplists where
  --datatype for a proplist
  type Plis = [(String,String)]
  --create an empty Plis

  --create an empty Plis
  pcreate :: Plis --returns a new, empty plist
  pcreate = []

  --pput
  pput :: String -> String -> Plis -> Plis --adds a prop, and its value, to an existing plist, returning the new plist
  pput p v pl = ((p,v):pdelete p pl)

  --pdelete
  pdelete :: String -> Plis -> Plis --deletes a prop and its value from a plist and returns the resulting plist
  pdelete p pl
          |null pl = []
          |p==q = rpl --found the item, answer is the rest
          |otherwise = ((q,v):pdelete p rpl) --rpl = rest of the proplist
          where ((q,v):rpl)=pl

  --pget
  pget :: String -> Plis -> Maybe String --retrieves data from plists: i.e. given a prop and a plist,
  pget q pl
      |null pl = Nothing -- it's not there
      |q==p = Just v -- found it
      |otherwise = pget q rpl --try the rest of the proplist
      where ((p,v):rpl) =pl

  --creating an example plist
  plis1 :: Plis
  plis1 = (pput "name" "James"
            (pput "phone" "07902409160"
              (pput "address" "12 Spring View Road, Sheffield" pcreate)))
