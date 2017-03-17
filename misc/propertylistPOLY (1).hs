{-***********************************
	propertylist.hs
  propertylist to handle the following functions:

  pcreate returns a new, empty plist

􏰀  pput adds a prop, and its value, to an existing plist, returning the new plist.

􏰀  pdelete deletes a prop and its value from a plist and returns the resulting plist.

  pget retrieves data from plists: i.e. given a prop and a plist, it returns the
  corresponding value (if the prop is present: otherwise we should return an
  indication that the search failed).


************************************-}
module Proplists where
  --datatype for a proplist
  type Dlis a = [a] --polymorphic type synonym

  phlis :: Dlis String -- a is Int
  phlis = ["A","A","B"]

  jlis :: Dlis String -- a is Int
  jlis = ["A","A","B"]
  deptData = (phlis)
