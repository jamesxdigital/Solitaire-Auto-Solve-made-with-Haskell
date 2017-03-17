{-***********************************
	tuples.hs

	Tuples are the counterpart to Lists. Lists have
  any number of elements, all of the same type.
  Tuples have a fixed number of elements of different
  types. Tuples are written with ()s rather than []s. e.g.

  (1,’a’) is a tuple of type (Int,Char)
  ([1,2], True, sqrt) is a tuple of type ([Int],Bool, Float -> Float)
************************************-}

roots :: Float -> Float -> Float -> (String,Float,String,Float)
roots a b c = let s = sqrt(b*b -4.0*a*c)
                  d = 2.0 * a
              in ("Positive:" , (-b+s)/d , " Negative:" , (-b-s)/d)
