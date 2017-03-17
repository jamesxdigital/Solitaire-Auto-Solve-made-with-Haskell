{-***********************************
	simpleif.hs
  finds the smallest of 3 numbers using if, then else
************************************-}
min_3 n1 n2 n3 = if n1<n2
  then
    (if n1<n3 then n1 else n3)
  else
    (if n2<n3 then n2 else n3)

    
