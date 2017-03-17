{-***********************************
	algebraicdata.hs

  Algebraic data type definitions are introduced by the keyword data
************************************-}

data Bool = False | True
data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter
              deriving (Eq,Show,Enum) -- Says that Seasons can be compared for identity (Eq type class):
                                      -- Enumerated types provide an ordering over the alternatives: if we say
                                        -- succ Spring ~> Summer

--functions using our defined algebraic data
weather :: Season -> Temp
weather Summer = Hot
weather _ = Cold
