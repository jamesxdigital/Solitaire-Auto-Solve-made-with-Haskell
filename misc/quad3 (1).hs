{-***********************************
	quad3.hs
	quadratic equation using 'where'
************************************-}
roots :: Float->Float->Float -> [Float]
roots a b c = [(-b+s)/d,(-b-s)/d]
              where s = sqrt(b*b -4.0*a*c)
                    d = 2.0*a
