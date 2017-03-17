{-***********************************
	quad1.hs
	quadratic equation
************************************-}
module Quad where
  quad :: Float -> Float -> Float -> [Float]
  quad a b c = [(-b + sqrt((b**2) - 4*a*c)) / (2*a),  (-b - sqrt((b**2) - 4*a*c)) / (2*a)]
