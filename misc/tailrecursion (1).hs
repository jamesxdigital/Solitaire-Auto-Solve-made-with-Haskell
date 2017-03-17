{-***********************************
	tailrecursion.hs
  Function member whose purpose is to tell us whether a given item is present in a given List

  Ord is the class of ordered types, which includes Int and Float, and for which >,<,<= etc. are defined.

  Eq is the type class supporting the identity operator == and ‘not equal’, /=. Types of class Ord
  will also support the functions defined for Eq, i.e. Ord inherits from Eq.

  Enum allows the type to be enumerated.

  Show allows instances of the type to be converted to strings (for printing).

  Read allows instances to be read from strings.
************************************-}
member :: Eq a => a-> [a] -> Bool
member x lis = if (null lis) then False --False if lis is empty
  else
    (if (x == head lis) then True --True if x is identical to the first item in lis
      else member x (tail lis)) --depending on whether x is a member of the rest of lis
