{-***********************************
	quad2.hs
	quadratic equation using

• let is followed by any number of local definitions.
• in separates these from the main expression.
• roots doesn’t deal properly with imaginary roots.
************************************-}
roots :: Float -> Float -> Float -> [Float]
roots a b c =
  let s = sqrt(b**2.0 - 4.0*a*c)
      d = 2.0 * a
  in [(-b+s)/d , (-b-s)/d]
