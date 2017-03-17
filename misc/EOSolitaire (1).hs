{-***********************************
EOSolitaire.hs
Game          : 8-Off Solitaire
Author        : James Milton 
last modified : 17/12/15
************************************-}
module EOSolitaire where

  import System.Random
  import Data.List
  import Data.Maybe
  import Debug.Trace

  data Suit = H|D|C|S
              deriving (Ord,Eq,Show,Enum)
  data Pip = A|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|JACK|QUEEN|KING
             deriving (Ord,Eq,Show,Enum)           
  type Card = (Pip,Suit)

  type Deck = [Card]

  type EOBoard = (Foundations, Columns, Reserve)

  type Foundations = [Card]

  type Columns = [[Card]]

  type Reserve = [Card]

  --sCard
  sCard :: Card->Card
  sCard (p,s) = ((succ p),s)

  --pCard
  pCard :: Card->Card
  pCard (p,s) =((pred p),s)

  --isAce
  isAce :: Card-> Bool
  isAce (p,_) = (p==A)

  --isKing
  isKing :: Card-> Bool
  isKing (p,_) = (p==KING)

  --pack
  pack :: Deck
  pack = [(p,s) | p <- [A .. KING], s <- [H .. S]]   

  --shuffle
  shuffle :: Int -> [Card]
  shuffle seed =  
    let
      gen = mkStdGen seed
      weights = take 52 (randoms gen :: [Int])
      dset = (map fst (sortBy (\ (_,w1)(_,w2)  -> (compare w1 w2))(zip pack weights)))
    in dset           

  --eODeal
  eODeal :: Int-> EOBoard
  eODeal seed = 
    let
      spack = shuffle seed
      cols  = [(take 6 spack),(take 6 (drop 6 spack)),(take 6 (drop 12 spack)),(take 6 (drop 18 spack)), 
               (take 6 (drop 24 spack)),(take 6 (drop 30 spack)),(take 6 (drop 36 spack)),(take 6 (drop 42 spack))]
      fnds  = []
      res   = [spack!!48,spack!!49,spack!!50,spack!!51]
    in (fnds,cols,res)          

  --example EOBoards
  b1@(f1,c1,r1) = eODeal 21
  b2@(f2,c2,r2) = toFoundations b1

  -- reserveAcesToFoundations
  reserveAcesToFoundations :: EOBoard -> EOBoard
  reserveAcesToFoundations (foundations,columns,reserve)
    | (null foundations) = ((filter isAce reserve),columns,(filter (not.isAce) reserve))
    | otherwise = (foundations,columns,reserve)

  --reserveToFoundations
  reserveToFoundations :: EOBoard -> EOBoard
  reserveToFoundations (foundations,columns,reserve) = 
      ((map (\f-> (if (not (isKing f))&&(elem (sCard f) reserve) then (sCard f) else f)) foundations), -- update fns
       columns, 
       (filter (\r-> (not(elem (pCard r) foundations )))reserve))  --update res

  --colAcesToFoundations
  colAcesToFoundations :: EOBoard -> EOBoard
  colAcesToFoundations (foundations,columns,reserve) = 
    (foundations ++ (filter isAce colHeads),
    (filter (not.null) (map (\col -> if (isAce (head col)) then (tail col) else col) columns)),
    reserve)
    where colHeads = map head columns
    
  --colHeadsToFoundations
  colHeadsToFoundations :: EOBoard -> EOBoard
  colHeadsToFoundations (foundations,columns,reserve) = 
    ((map (\f-> if (not(isKing f))&&(elem (sCard f) colHeads) then (sCard f) else f) foundations), -- **f can't be king update foundations
     (filter (not.null) -- update columns
             (map (\col@(hc:rc)-> if (elem (pCard hc) foundations) then rc else col)columns)), -- may have emptied a column
     reserve) 
    where colHeads = map head columns 
   
  --toFoundationsAUX
  toFoundationsAUX :: EOBoard -> EOBoard
  toFoundationsAUX eoboard
    | eoboard/=colAceFBd = toFoundationsAUX colAceFBd
    | eoboard/=resFBd = toFoundationsAUX resFBd
    | eoboard/=colHeadFBd = toFoundationsAUX colHeadFBd
    | otherwise = eoboard
    where colAceFBd = colAcesToFoundations eoboard
          resFBd = reserveToFoundations eoboard
          colHeadFBd = colHeadsToFoundations eoboard   

  --toFoundations
  toFoundations :: EOBoard -> EOBoard
  toFoundations eoboard = toFoundationsAUX (reserveAcesToFoundations eoboard)


  {-*************
  AUTO-PLAY
  ***************-}

  --getReserve (helper to return reserve when dealing with EOBoard)
  getReserve :: EOBoard -> Reserve
  getReserve (foundations,columns,reserve) = reserve

  --getColumns (helper to return columns when dealing with EOBoard)
  getColumns :: EOBoard -> Columns
  getColumns (foundations,columns,reserve) = columns    

  --getColumnsHeads (given an EOBoard, returns a list of the top cards (heads) in a column)
  getColumnsHeads :: EOBoard -> Deck
  getColumnsHeads (foundations,columns,reserve) = map (\n -> (head n)) (columns) 

  --removeCol (given a card that is in the EOBoard's columns, will return an EOBoard with that card removed)
  removeCol:: Card -> EOBoard -> EOBoard --(down) uses a filter remove card from any of the columns
  removeCol card (foundations,columns,reserve) = (foundations, [f|l <- (columns), let f = filter (/= card) l], reserve)

  --removeRes (given a card that is in the EOBoard's reserve, will return an EOBoard with that card removed)
  removeRes:: Card -> EOBoard -> EOBoard
  removeRes card (foundations,columns,reserve) =  (foundations, columns, delete card (reserve))

  --kingAtColHead (given columns, will check if head of any columns contain a King, and if so return True)
  kingAtColHead :: Columns -> Bool 
  kingAtColHead [] = False
  kingAtColHead columns
    | ((length headcolumn) > 1) && (isKing (head headcolumn))  = True
    | otherwise = kingAtColHead (filter (not . null)restcolumns)
    where (headcolumn:restcolumns) = columns

  --getKingAtColHead (if there was a King at the head of the column, getKingAtColHead will return that King card)
  getKingAtColHead :: Columns -> Card
  getKingAtColHead columns 
    | isKing (head headcolumn) && ((length headcolumn) > 1)= head headcolumn
    | otherwise = getKingAtColHead (filter (not . null)restcolumns)
    where (headcolumn:restcolumns) = columns

  --kingAtReserve (will return true if any of the cards in the reserve are a King)
  kingAtReserve :: Reserve -> Bool
  kingAtReserve [] = False
  kingAtReserve reserve
    | isKing headreserve = True
    | otherwise = kingAtReserve restreserve
    where (headreserve:restreserve) = reserve

  --getKingAtRes (if there was a King in the reserve, getKingAtRes will return that King card)
  getKingAtRes :: Reserve -> Card
  getKingAtRes reserve
    | isKing headreserve = headreserve
    | otherwise =  getKingAtRes restreserve
    where (headreserve:restreserve) = reserve

  --isCardMovable (given a card and a deck(i.e. foundation heads), will return True if card if prececcesor of any 
  isCardMovable :: Card -> Deck -> Bool -- of the cards in the given deck)
  isCardMovable (A,_) _ = True
  isCardMovable card [] = False --if given an empty deck, successor will never exist
  isCardMovable card deckFoundations 
    | pCard card == cardF = True --using the maybe type
    | otherwise = isCardMovable card restfoundation --recerse through rest of deck (foundation heads)
      where ((cardF):restfoundation) = deckFoundations 

  --isSecondCardKing (given columns, will return true if any of the columns second from top card is a King)
  isSecondCardKing :: Columns -> Bool
  isSecondCardKing [] = False
  isSecondCardKing columns 
    | length headcolumn == 1 = isSecondCardKing (filter (not . null)restcolumns)
    | isKing(headcolumn !! 1) && (length headcolumn > 2) = True
    | otherwise = isSecondCardKing (filter (not . null)restcolumns)
    where (headcolumn:restcolumns) = columns 

  --getMoveableCardSecondKing (if any of the second from top cards in columns was a King card 
  getMoveableCardSecondKing :: Columns -> Card --getMoveableCardSecondKing will return the card before in column)
  getMoveableCardSecondKing columns 
    | length headcolumn == 1 = getMoveableCardSecondKing (filter (not . null)restcolumns) 
    | isKing(headcolumn !! 1) && (length headcolumn > 2) = (headcolumn !! 0)
    | otherwise = getMoveableCardSecondKing (filter (not . null)restcolumns) 
    where (headcolumn:restcolumns) = columns 

  --allMovableCard (given two decks (foundation heads and available cards) returns a deck of cards  
  allMovableCard :: Deck -> Deck -> Deck    --that are successors of any cards in foundation deck)
  allMovableCard _ [] = [] --if either decks are empty, just return empty list
  allMovableCard [] _ = []
  allMovableCard deckFoundations deckAvailable
      | null deckAvailable = (filter (not.isAce)restavailable)
      | isCardMovable cardA deckFoundations = (filter (not.isAce)((cardA): allMovableCard deckFoundations restavailable))
      | otherwise = allMovableCard deckFoundations restavailable
      where ((cardA):restavailable) = deckAvailable 

  --addToColHead (given a card that can legally go onto column head, will return column with card added to appropriate column)
  addToColHead :: Card -> Columns -> Columns
  addToColHead card columns = map (\x -> (if ((head x) == card) then (pCard card : x) else x)) columns

  --isSecondCardSuccessor (given columns, will return True if any second from top card is a successor of any of the top cards)
  isSecondCardSuccessor :: Columns -> EOBoard -> Bool
  isSecondCardSuccessor [] _ = False
  isSecondCardSuccessor columns eoboard
    | length headcolumn == 1 = isSecondCardSuccessor restcolumns eoboard
    | isKing secondCard = isSecondCardSuccessor restcolumns eoboard
    | sCard(secondCard) `elem` columnHeads =  True
    | otherwise = isSecondCardSuccessor restcolumns eoboard
    where (headcolumn:restcolumns) = columns 
          columnHeads = getColumnsHeads eoboard
          secondCard = (headcolumn !! 1)

  --getSecondCardSuccessor (if there was a card second from top in a column pile that was a successor of a any of the head
  getSecondCardSuccessor :: Columns -> EOBoard -> Card  -- column piles, getSecondCardSuccessor will return the card before it)
  getSecondCardSuccessor columns eoboard
    | length headcolumn == 1 = getSecondCardSuccessor restcolumns eoboard
    | isKing secondCard = getSecondCardSuccessor restcolumns eoboard
    | sCard(secondCard) `elem` columnHeads = (headcolumn !! 0)
    | otherwise = getSecondCardSuccessor restcolumns eoboard
    where (headcolumn:restcolumns) = columns 
          columnHeads = getColumnsHeads eoboard
          secondCard = (headcolumn !! 1)

  --columnOfOne (given columns, if there contains a column which only contains one card and is not a King, return True)
  columnOfOne :: Columns -> EOBoard -> Bool  
  columnOfOne [] _ = False
  columnOfOne columns eoboard 
    | (length headcolumn == 1) && (not (isKing(headcolumn !! 0))) = True
    | otherwise =  columnOfOne restcolumns eoboard
    where (headcolumn:restcolumns) = columns 
          columnHeads = getColumnsHeads eoboard

  --getColumnOfOne (if there was a column of length one and the card was not a King, getColumnOfOne will return that card)
  getColumnOfOne :: Columns -> EOBoard -> Card
  getColumnOfOne columns eoboard 
    | (length headcolumn == 1) && (not (isKing(headcolumn !! 0))) = (headcolumn !! 0)
    | otherwise =  getColumnOfOne restcolumns eoboard
    where (headcolumn:restcolumns) = columns 
          columnHeads = getColumnsHeads eoboard  

  --isNthCardMoveable (if the nth card in a column can be moved onto the foundation, then return True)
  isNthCardMoveable :: Columns -> Foundations -> Int -> Bool
  isNthCardMoveable [] _ _ = False
  isNthCardMoveable columns foundation nth
    | length headcolumn <= (nth - 1) = isNthCardMoveable (filter (not . null)restcolumns) foundation nth
    | isCardMovable (headcolumn !! (nth - 1)) foundation = True
    | otherwise = isNthCardMoveable (filter (not . null)restcolumns) foundation nth
    where (headcolumn:restcolumns) = columns 

  --getNthMoveableCard (if there was a card nth in a column that can be moved to teh foundation, then getNthMoveableCard will
  getNthMoveableCard :: Columns -> Foundations -> Int -> Card -- return head of the column where the nth card is)
  getNthMoveableCard columns foundation nth
    | length headcolumn <= (nth -1) = getNthMoveableCard (filter (not . null)restcolumns) foundation nth
    | isCardMovable (headcolumn !! (nth -1)) foundation = (headcolumn !! 0)
    | otherwise = getNthMoveableCard (filter (not . null)restcolumns) foundation nth
    where (headcolumn:restcolumns) = columns 

  --moveKingToEmptyColumn (Move King from head of column to empty column)
  moveKingToEmptyColumn :: EOBoard -> EOBoard
  moveKingToEmptyColumn eoboard@(foundations,columns,reserve) = (foundations, 
                                                        (getColumns (removeCol (getKingAtColHead (columns)) eoboard)) ++ [[(getKingAtColHead (columns))]], 
                                                        reserve)

  --moveKingResToEmptyColumn (Move King from reserve to empty column)
  moveKingResToEmptyColumn :: EOBoard -> EOBoard
  moveKingResToEmptyColumn eoboard@(foundations,columns,reserve) = (foundations, 
                                                        columns ++ [[(getKingAtRes reserve)]], 
                                                        (getReserve(removeRes (getKingAtRes reserve) eoboard)))

  --moveResPredColHead (Move card in reserve that is predeccesor of once of column heads onto that column)
  moveResPredColHead :: EOBoard -> EOBoard
  moveResPredColHead eoboard@(foundations,columns,reserve) = (foundations, 
                                                        (addToColHead ((head(allMovableCard reserve (getColumnsHeads eoboard)))) columns),
                                                        getReserve(removeRes (pCard(head (allMovableCard reserve (getColumnsHeads eoboard)))) eoboard))

  --moveColsSucc (Move any cards between the columns if they are successors)
  moveColsSucc :: EOBoard -> EOBoard
  moveColsSucc eoboard@(foundations,columns,reserve) = (foundations,
                                                        (addToColHead ((head(allMovableCard (getColumnsHeads eoboard) (getColumnsHeads eoboard)))) 
                                                          (filter (not.null)(getColumns(removeCol (pCard(head(allMovableCard (getColumnsHeads eoboard) (getColumnsHeads eoboard)))) eoboard)))) ,
                                                        reserve)

  --move2ndKingRes (If the second card in column is a King and there is a spare column and reserve has space, move first card in that column to reserve)
  move2ndKingRes :: EOBoard -> EOBoard
  move2ndKingRes eoboard@(foundations,columns,reserve) = (foundations, 
                                                        getColumns(( removeCol(getMoveableCardSecondKing columns) eoboard)), 
                                                        reserve ++ [getMoveableCardSecondKing columns])

  --move2ndColSucc (If second card in a column is a successor of another column head, move first card to reserve)
  move2ndColSucc :: EOBoard -> EOBoard
  move2ndColSucc eoboard@(foundations,columns,reserve) = (foundations, 
                                                        (getColumns(removeCol(getSecondCardSuccessor columns eoboard) eoboard) ),
                                                        reserve ++ [getSecondCardSuccessor columns eoboard])

  --moveSingleColRes (If there is a column with just one item and it's not a king, move card to reserve)
  moveSingleColRes :: EOBoard -> EOBoard
  moveSingleColRes eoboard@(foundations,columns,reserve) = (foundations, 
                                                        (filter (not.null)(getColumns(removeCol(getColumnOfOne columns eoboard) eoboard))),  
                                                        reserve ++ [getColumnOfOne columns eoboard]  )

  --moveNthCardHeadRes (If Nth card in column can be moved to foundation, move the head of the column the Nth card is in to reserve)
  moveNthCardHeadRes :: EOBoard -> Int -> EOBoard
  moveNthCardHeadRes eoboard@(foundations,columns,reserve) nth = (foundations, 
                                                                getColumns(removeCol (getNthMoveableCard columns foundations nth) eoboard) , 
                                                                reserve ++ [getNthMoveableCard columns foundations nth])

  --findMoves (given an EOBoard, it will return only a list EOBoards with legal moves made, ordered by the best move at the top of list to least-best at the bottom)
  findMoves :: EOBoard -> [EOBoard]
  findMoves eoboard@(foundations,columns,reserve) = 
    filter (\l->l/=([],[],[]))  --if the list contains an empty EOBoard, then filter them out.
      --if toFoundations is a different EOBoard to before, then return that EOBoard at the best move to make
      [(if eoboard /= (toFoundations eoboard) then (toFoundations eoboard) else ([],[],[])),
        --the there is atleast once space in reserve, and the 2nd card in a column can be moved to foundation, then move first card in column to reserve
        (if (((length reserve) <= 7) && (isNthCardMoveable columns foundations 2)) then (moveNthCardHeadRes eoboard 2) else ([],[],[])),
          --same as above, except with (n+1) card in the column. Also with (n+1) spare reserve spaces need.
          (if (((length reserve) <= 6) && (isNthCardMoveable columns foundations 3)) then (moveNthCardHeadRes eoboard 3) else ([],[],[])),
            (if (((length reserve) <= 5) && (isNthCardMoveable columns foundations 4)) then (moveNthCardHeadRes eoboard 4) else ([],[],[])),
              (if (((length reserve) <= 4) && (isNthCardMoveable columns foundations 5)) then (moveNthCardHeadRes eoboard 5) else ([],[],[])),
                (if (((length reserve) <= 3) && (isNthCardMoveable columns foundations 6)) then (moveNthCardHeadRes eoboard 6) else ([],[],[])),
                  (if (((length reserve) <= 2) && (isNthCardMoveable columns foundations 7)) then (moveNthCardHeadRes eoboard 7) else ([],[],[])),
                    (if (((length reserve) <= 1) && (isNthCardMoveable columns foundations 8)) then (moveNthCardHeadRes eoboard 8) else ([],[],[])),
                      --if there is atleast one empty column, move King from reserve into empty column
                      (if (((length columns) <= 7) && (kingAtReserve (reserve))) then  moveKingResToEmptyColumn eoboard else ([],[],[])),
                        --if there is atleast one empty column, move King from column head into empty column
                        (if (((length columns) <= 7) && (kingAtColHead (columns))) then moveKingToEmptyColumn eoboard else ([],[],[])),
                          --if there is atleast one empty column and reserve space and a King 2nd in column, move first card in column to reserve
                          (if (((length reserve) <= 7) && (isSecondCardKing columns)) && ((length columns) <= 7) then move2ndKingRes eoboard else ([],[],[])),
                            --if there is atleast one empty column and reserve space and a successor to column head 2nd in column, move first card in column to reserve
                            (if (((length reserve) <= 7) && (isSecondCardSuccessor columns eoboard)) then move2ndColSucc eoboard else ([],[],[])),
                              -- if there are any successors between column heads, then move successor column head to the other column head
                              (if (length (allMovableCard (getColumnsHeads eoboard) (getColumnsHeads eoboard)) /= 0) then moveColsSucc eoboard else ([],[],[])),
                                -- if there any cards in the reserve that can be put onto a column head, move card from reserve onto column head
                                (if (length (allMovableCard reserve (getColumnsHeads eoboard)) /= 0) then moveResPredColHead eoboard else ([],[],[])),
                                  --if there is atleast one space in the reserve and there is a column with once card that is not a king, move that card to reserve
                                  (if (((length reserve) <= 7) && (columnOfOne columns eoboard)) then moveSingleColRes eoboard else ([],[],[]))]


  --chooseMove (given an EOBoard, if there are no moves available in findMoves (i.e. an empty list), then return Nothing                           
  chooseMove :: EOBoard -> Maybe EOBoard -- or if the length of find moves is atleast of length 1, then return the first EOBoard in the list)
  chooseMove eoboard@(foundations,columns,reserve)
    | length (findMoves eoboard) >= 1 = Just ((head (findMoves eoboard)))
    | otherwise = Nothing 

  --score (given an EOBoard, it will return an Int corresponding to the number of cards in the foundation of that EOBoard)
  score :: EOBoard -> Int 
  score (foundations, columns, reserve) = read (show (52- (length reserve) - (foldr (+) 0 (map length columns)))) :: Int 

  --eOGame (given an EOBoard, eOGame will play that EOBoard and will then return the score of which it managed to get to)
  eOGame :: EOBoard -> Int
  eOGame eoboard@(foundations,columns,reserve) =  
   if ((null columns)&&(null reserve)) then (score eoboard)
      else if (isJust (chooseMove eoboard)) then (eOGame (resMaybe (chooseMove eoboard)))
        else (score eoboard)

  --eOExptAUX (auxiliary function for eOExpt which when given an intiall random seed, will then return a list of scores for eODeal games 
  eOExptAUX :: Int -> [Int] -- from the initial seed to the upper limit. (E.g. eOExptAUX 1 -> (list of scores with seed 1 to 100)))
  eOExptAUX initialSeed = map (\x -> (eOGame(eODeal x))) [(initialSeed)..(initialSeed+99)]

  --average (given a list of numbers, such as the output for eOExptAUX, will then return the average of all numbers in list)
  average :: (Real a, Fractional b) => [a] -> b
  average xs = realToFrac (sum xs) / genericLength xs

  --eOExpt (given an initial seed, eOExpt will play 100 eODeal games from the inital seed, the returning a 
  eOExpt :: Int -> (Int,Float) -- a tuple in the form (number of wins, average score (using the average function above)))
  eOExpt initialSeed = (numberOfWins, averageScore)
    where numberOfWins = (length (filter (\n -> (n == 52)) scores)) 
          scores = (eOExptAUX initialSeed)
          averageScore = average (scores :: [Int])

  --resMaybe (Maybe helper )               
  resMaybe :: (Maybe a) -> a
  resMaybe (Just x) = x 