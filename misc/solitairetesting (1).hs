{-***********************************
        FOR TESTING USE ONLY

solitairetesting.hs
Author        : James Milton 
last modified : 21/11/15

        FOR TESTING USE ONLY
************************************-}

module Testing where
  import Solitaire
  import Debug.Trace

  --toFoundationsTesting
  --This alteration of the toFoundations fucntions incorporates trace from the Debug.Trace package. When ran, it 
  toFoundationsTesting :: EOBoard -> EOBoard    --will list each card that has been moved and then show the resulting eoboard
  toFoundationsTesting eoboard
      |deckContainsAce (getColumnsHeads (getColumns eoboard)) = trace (show(getLowestCard (getColumnsHeads (getColumns eoboard)))) (toFoundationsTesting newAceCol)
      |deckContainsAce (getReserve eoboard) = trace (show(getLowestCard (getReserve eoboard)))  (toFoundationsTesting newAceRes)
      |allMovableCard (getFoundationHeads (getFoundation eoboard)) (allAvailableCards eoboard) == [] = eoboard
      |deckContainSucc (getReserve eoboard) (getFoundationHeads (getFoundation eoboard)) = trace (show(reserveCard))  (toFoundationsTesting newSuccRes)
      |deckContainSucc (getColumnsHeads (getColumns eoboard)) (getFoundationHeads (getFoundation eoboard))= trace (show(columnCard))  (toFoundationsTesting newSuccCol)
      where newAceCol = removeCol (getLowestCard (getColumnsHeads (getColumns eoboard)))(addAceToFoundation eoboard (getLowestCard (getColumnsHeads (getColumns eoboard))))
            newAceRes = removeRes (getLowestCard (getReserve eoboard)) (addAceToFoundation eoboard (getLowestCard (getReserve eoboard)))
            newSuccRes = removeRes reserveCard (addCardToFoundation eoboard reserveCard)
            newSuccCol = removeCol columnCard (addCardToFoundation eoboard columnCard)
            reserveCard = head (allMovableCard (getFoundationHeads (getFoundation eoboard)) (getReserve eoboard))
            columnCard = head (allMovableCard (getFoundationHeads (getFoundation eoboard)) (getColumnsHeads (getColumns eoboard)))


  eODealTest1 :: EOBoard --this EOBoard will return a full foundation and empty columns when used with toFoundationsTesting
  eODealTest1 = EOBoard {foundations = [], 
                        columns = [[(SIX,D),(FIVE,D),(FOUR,D),(THREE,D),(TWO,D),(A,D)],
                        [(QUEEN,D),(JACK,D),(TEN,D),(NINE,D),(EIGHT,D),(SEVEN,D)],
                        [(FIVE,H),(FOUR,H),(THREE,H),(TWO,H),(A,H),(KING,D)],
                        [(JACK,H),(TEN,H),(NINE,H),(EIGHT,H),(SEVEN,H),(SIX,H)],
                        [(FOUR,S),(THREE,S),(TWO,S),(A,S),(KING,H),(QUEEN,H)],
                        [(TEN,S),(NINE,S),(EIGHT,S),(SEVEN,S),(SIX,S),(FIVE,S)], 
                        [(THREE,C),(TWO,C),(A,C),(KING,S),(QUEEN,S),(JACK,S)],
                        [(NINE,C),(EIGHT,C),(SEVEN,C),(SIX,C),(FIVE,C),(FOUR,C)]],
                        reserve = [(KING,C),(JACK,C),(TEN,C),(QUEEN,C)]}

  eODealTest2 :: EOBoard --this EOBoard will return empty foundation, columns and reserve when used with toFoundationsTesting
  eODealTest2 = EOBoard {foundations = [], 
                        columns = [],
                        reserve = []}

  eODealTest3 :: EOBoard --this EOBoard will return an eoboard where five cards can be moved 
  eODealTest3 = EOBoard {foundations = [], 
                        columns = [[(THREE,S),(TWO,C),(FOUR,S),(SEVEN,D),(A,D),(A,H)],
                        [(JACK,H),(EIGHT,C),(NINE,D),(THREE,C),(KING,D),(NINE,C)],
                        [(SIX,S),(FIVE,H),(A,C),(SIX,C),(FOUR,D),(KING,C)],
                        [(SEVEN,C),(EIGHT,D),(TWO,S),(TEN,D),(KING,H),(THREE,D)],
                        [(FIVE,C),(QUEEN,H),(FOUR,C),(SEVEN,H),(JACK,S),(TWO,D)],
                        [(THREE,H),(FIVE,S),(SEVEN,S),(TEN,S),(TWO,H),(JACK,C)],
                        [(QUEEN,D),(KING,S),(JACK,D),(EIGHT,S),(NINE,H),(FOUR,H)],
                        [(NINE,S),(SIX,H),(QUEEN,C),(TEN,H),(FIVE,D),(EIGHT,H)]], 
                        reserve = [(TEN,C),(SIX,D),(A,S),(QUEEN,S)]}

  eODealTest4 :: EOBoard  --this EOBoard will return an eoboard where four cards can be moved 
  eODealTest4 = EOBoard  {foundations = [], 
                         columns = [[(TEN,S),(FOUR,C),(TEN,C),(TWO,D),(QUEEN,C),(JACK,D)],
                         [(JACK,H),(FIVE,D),(SIX,C),(EIGHT,H),(THREE,S),(QUEEN,D)],
                         [(NINE,S),(SEVEN,D),(QUEEN,S),(A,S),(QUEEN,H),(THREE,C)],
                         [(NINE,D),(SIX,S),(KING,S),(NINE,H),(SIX,H),(FOUR,S)],
                         [(SEVEN,H),(A,H),(JACK,S),(TWO,S),(FIVE,S),(KING,C)],
                         [(NINE,C),(SEVEN,S),(EIGHT,S),(KING,D),(TEN,D),(TWO,C)],
                         [(EIGHT,C),(FOUR,H),(FIVE,C),(TWO,H),(KING,H),(SEVEN,C)],
                         [(THREE,H),(SIX,D),(FOUR,D),(JACK,C),(EIGHT,D),(THREE,D)]], 
                         reserve = [(A,C),(FIVE,H),(A,D),(TEN,H)]}

  eODealTest5 :: EOBoard --this EOBoard will return an eoboard where 1 card can be moved 
  eODealTest5 = EOBoard {foundations = [], 
                         columns = [[(TEN,C),(FOUR,C),(QUEEN,D),(TWO,C),(EIGHT,H),(THREE,H)],
                         [(JACK,D),(FIVE,H),(A,C),(SEVEN,S),(JACK,H),(A,S)],
                         [(FOUR,H),(NINE,S),(THREE,C),(SIX,H),(EIGHT,D),(SIX,D)],
                         [(FIVE,S),(A,H),(TWO,D),(TWO,S),(TWO,H),(JACK,C)],
                         [(SEVEN,D),(TEN,D),(FOUR,S),(SIX,C),(SEVEN,C),(QUEEN,C)],
                         [(NINE,C),(SEVEN,H),(TEN,H),(KING,D),(KING,H),(THREE,S)],
                         [(KING,C),(SIX,S),(QUEEN,S),(QUEEN,H),(FIVE,D),(NINE,H)],
                         [(TEN,S),(A,D),(NINE,D),(EIGHT,C),(THREE,D),(KING,S)]], 
                         reserve = [(FOUR,D),(EIGHT,S),(JACK,S),(FIVE,C)]}
