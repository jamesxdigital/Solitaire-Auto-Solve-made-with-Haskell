{-***********************************
solitaire.hs
Game          : 8-Off Solitaire
Author        : James Milton 
last modified : 21/11/15
************************************-}
module Solitaire where
  -- System.Random used for random shuffle | Data.List used for manipulation of lists
  import System.Random
  import Data.List

  data Suit = H|D|C|S --Heart|Diamond|Club|Spade
              deriving (Ord,Eq,Show,Enum)
  data Pip = A|TWO|THREE|FOUR|FIVE|SIX|SEVEN|EIGHT|NINE|TEN|JACK|QUEEN|KING
             deriving (Ord,Eq,Show,Enum) -- Using Ord,Eq,Show,Enum to enable pip to be ordered
  type Card = (Pip,Suit)                 -- (i.e. A<KING) and to allow type to be equal or not,
                                         -- to allow it to be shown in console and to see what 
  type Deck = [Card]                     -- type goes after which (i.e. THREE is after TWO) respectively.

  type Reserve = Deck --reserve does not have list of decks, just a list of cards (deck)

  type Columns = [Deck] --columns will consist of 8 decks of cards

  type Foundations = [Deck] --foundation will consist of 4 decks of cards
  --Using record syntax to represent the EOBoard. Allows user to see which part of the EOBoard is shown
  data EOBoard = EOBoard {foundations::Foundations, columns::Columns , reserve::Reserve} deriving (Show)

  --pack (unshuffled pack of 52 cards)
  pack :: Deck
  pack = [(p,s) | p <- [A .. KING], s <- [H .. S]] --because Suit and Pip derive Enum, this syntax can be used

  --pCard (takes a card and returns the predecessor)
  pCard :: Card -> Maybe Card --not every card has a predecessor
  pCard card
      |pip == A = Nothing -- ace cards do not have a predecessor
      |otherwise = Just ((pred pip,suit)) --return the predeccesor of pip and the exisitng suit
      where (pip,suit) = card

  --sCard (takes a card and returns the successor)
  sCard :: Card -> Maybe Card --not every card has a successor
  sCard card
      |pip == KING = Nothing --king cards do not have successor
      |otherwise = Just ((succ pip,suit)) --return the successor of pip and the exisitng suit
      where (pip,suit) = card

  --isAce (takes a card, returns boolean depending if ace of not)
  isAce :: Card -> Bool
  isAce card
      |pip == A = True --if the pip of the card is A, then it will be an ace. Return True
      |otherwise = False --any other pip will not be an ace card, return false
      where (pip,suit) = card

  --isKing (takes a card, returns boolean depending if king of not)
  isKing :: Card -> Bool
  isKing card
      |pip == KING = True --if the pip of the card is KING, then it will be an King. Return True
      |otherwise = False --any other pip will not be an KING card, return false
      where (pip,suit) = card

  --shuffle (takes a deck then returns a shuffled deck)
  shuffle :: Deck -> Deck
  shuffle deck =
    let rlis = take (length deck) (randoms (mkStdGen 300):: [Int]) --create list of random ints of length of deck
        zlis = zip deck rlis  --zip the each member of deck and random list into a tuple
        slis = mergesort (\(_,n1) (_,n2)->n1<n2) zlis --perform merge sort on random ints, ordring assending order
    in  (map fst slis) --using a mapping function, obtain the fst (first) member of the tuple (the card), 
                       --stripping the random number

  --eODeal (returns an eight off board with shuffled cards)
  eODeal :: EOBoard --eODeal consists of predfined foundations, columns and reservve that have been assigned random cards below
  eODeal = EOBoard {foundations=defaultFoundations, columns=randomColumns, reserve=randomReserve}
  
  --defaultFoundations (initially starts empty)
  defaultFoundations =  []
  
  --randomColumns (takes the random cards and assignes them into seperate columns using drop and take)
  randomColumns = [take 6 (shuffle pack),
                  drop 6(take 12 (shuffle pack)),
                  drop 12(take 18 (shuffle pack)),
                  drop 18(take 24 (shuffle pack)),
                  drop 24(take 30 (shuffle pack)),
                  drop 30(take 36 (shuffle pack)),
                  drop 36(take 42 (shuffle pack)),
                  drop 42(take 48 (shuffle pack))]

  --randomReserve (takes the last of the random cards and assign to reserve)             
  randomReserve = drop 48(take 52 (shuffle pack))


  --eODealTest (test eight off board)
  eODealTest :: EOBoard --this EOBoard will return a full foundation and empty columns when used with toFoundations
  eODealTest = EOBoard {foundations = [], 
                        columns = [[(SIX,D),(FIVE,D),(FOUR,D),(THREE,D),(TWO,D),(A,D)],
                        [(QUEEN,D),(JACK,D),(TEN,D),(NINE,D),(EIGHT,D),(SEVEN,D)],
                        [(FIVE,H),(FOUR,H),(THREE,H),(TWO,H),(A,H),(KING,D)],
                        [(JACK,H),(TEN,H),(NINE,H),(EIGHT,H),(SEVEN,H),(SIX,H)],
                        [(FOUR,S),(THREE,S),(TWO,S),(A,S),(KING,H),(QUEEN,H)],
                        [(TEN,S),(NINE,S),(EIGHT,S),(SEVEN,S),(SIX,S),(FIVE,S)], 
                        [(THREE,C),(TWO,C),(A,C),(KING,S),(QUEEN,S),(JACK,S)],
                        [(NINE,C),(EIGHT,C),(SEVEN,C),(SIX,C),(FIVE,C),(FOUR,C)]],
                        reserve = [(KING,C),(JACK,C),(TEN,C),(QUEEN,C)]}

  --getReserve (given an eoboard, returns reserve )
  getReserve :: EOBoard -> Reserve
  getReserve eoboard = reserve eoboard 

  --getFoundation (given an eoboard, returns foundation)
  getFoundation :: EOBoard -> Foundations 
  getFoundation eoboard = foundations eoboard
  
  --getFoundationHeads (given the foundations, returns a list of the top cards (heads))
  getFoundationHeads :: Foundations -> Deck
  getFoundationHeads foundations = map (\n -> (last n)) (foundations) --using a map to obtain top (last) cards in list

  --getColumns (given an eoboard, returns columns)
  getColumns :: EOBoard -> Columns
  getColumns eoboard = filter(\x -> x /= []) (columns eoboard) --filters out any empty lists to prevent errors

  --getColumnsHeads (given the columns, returns a list of the top cards (heads))
  getColumnsHeads :: Columns -> Deck
  getColumnsHeads columns = map (\n -> (last n)) (columns) --using a map to obtain top (last) cards in list
  
  --deckContainsAce (given a deck (i.e. reserve), returns true if reserve contains ace card)
  deckContainsAce :: Deck -> Bool
  deckContainsAce [] = False --if empty return false
  deckContainsAce reserve
      | A==pip = True --item desired is in head, return true
      | otherwise = deckContainsAce restreserve --recurse through rest of bag
      where ((pip,_):restreserve) = reserve

  --deckContainSucc (given a deck(i.e. reserve), returns true if successor exists within other deck)
  deckContainSucc :: Deck -> Deck -> Bool
  deckContainSucc [] _ = False  --if either deck are empty, return false
  deckContainSucc _ [] = False
  deckContainSucc reserve moveable
      |isCardMovable cardF moveable = True
      |otherwise = deckContainSucc restreserve moveable --recerse through rest of bag
      where (cardM:restmoveable) = moveable
            (cardF:restreserve) = reserve  

  --getLowestCard (given a deck, returns the lowest card pip in deck)
  getLowestCard :: Deck -> Card
  getLowestCard deck = head (sort deck) --lowest card will be at head in certain situations

  --removeCol (given a card and an eoboard, it will return an eoboard with card removed from column)
  removeCol:: Card -> EOBoard -> EOBoard --(down) uses a filter remove card from any of the columns
  removeCol card eoboard = eoboard { columns = [f|l <- (getColumns eoboard), let f = filter (/= card) l]}

  --removeRes (given a card and an eoboard, it will return an eoboard with card removed from reserve)
  removeRes:: Card -> EOBoard -> EOBoard --(down) using delete function to remove card from list (reserve)
  removeRes card eoboard = eoboard { reserve = delete card (getReserve eoboard) }

  --addAceToFoundation (given an eoboard and an ace card, it will add card to foundation inside a new list)
  addAceToFoundation :: EOBoard -> Card -> EOBoard
  addAceToFoundation eoboard card = eoboard { foundations = [card]: getFoundation eoboard}

  --addCardToFoundation (given an eoboard and a card, it will add card to foundation into pile with corresponding suit)
  addCardToFoundation :: EOBoard -> Card -> EOBoard --(down) main function uses aux function to obtain foundations
  addCardToFoundation eoboard card = eoboard { foundations = (addCardToFoundationAUX (getFoundation eoboard) card)}

  --addCardToFoundationAUX (auxiliary function for addCardToFoundation)
  addCardToFoundationAUX :: Foundations -> Card -> Foundations
  addCardToFoundationAUX foundation card 
      | suit == suitcard = [cardA++[card]]++restfoundation --if suit of card == suit of foundation list, add to list and display the rest
      | otherwise = [cardA]++(addCardToFoundationAUX restfoundation card) --otherwise display initial lists then recerse
      where found@( cardA@((_,suit):_) :restfoundation) = foundation
            (_,suitcard) = card

  --allAvailableCards (given an eoboard, it will return a deck containing all the cards that could move)
  allAvailableCards :: EOBoard -> Deck --(down) all available cards are those that are in the reserve and top of each columns 
  allAvailableCards eoboard  = (getColumnsHeads (getColumns eoboard)) ++ (getReserve eoboard)

  --isCardMovable (given a card and a deck (foundation heads) will return a bool if card successor of any element in deck)
  isCardMovable :: Card -> Deck -> Bool
  isCardMovable card [] = False --if given an empty deck, successor will never exist
  isCardMovable card deckFoundations 
      | pCard card == Just cardF = True --using the maybe type
      | otherwise = isCardMovable card restfoundation --recerse through rest of deck (foundation heads)
      where ((cardF):restfoundation) = deckFoundations 

  --allMovableCard (given two decks (foundation heads and available cards) returns a deck of cards 
  allMovableCard :: Deck -> Deck -> Deck                  --that are successors of any cards in foundation deck)
  allMovableCard _ [] = [] --if either decks are empty, just return empty list
  allMovableCard [] _ = []
  allMovableCard deckFoundations deckAvailable
      | null deckAvailable = restavailable --(using previous Boolean function on each card then recsersing through rest of deck)
      | isCardMovable cardA deckFoundations = ((cardA): allMovableCard deckFoundations restavailable)
      | otherwise = allMovableCard deckFoundations restavailable
      where ((cardA):restavailable) = deckAvailable 

  --toFoundations (given an eoboard, it will return an eoboard where all possible moved have been made to the foundation)
  toFoundations :: EOBoard -> EOBoard --COMMENTS ON HOW toFoundations WORKS BELOW
  toFoundations eoboard
      |deckContainsAce (getColumnsHeads (getColumns eoboard)) = toFoundations newAceCol
      |deckContainsAce (getReserve eoboard) = toFoundations newAceRes
      |allMovableCard (getFoundationHeads (getFoundation eoboard)) (allAvailableCards eoboard) == [] = eoboard
      |deckContainSucc (getReserve eoboard) (getFoundationHeads (getFoundation eoboard)) = toFoundations newSuccRes
      |deckContainSucc (getColumnsHeads (getColumns eoboard)) (getFoundationHeads (getFoundation eoboard))= toFoundations newSuccCol
      where newAceCol = removeCol (getLowestCard (getColumnsHeads (getColumns eoboard)))(addAceToFoundation eoboard (getLowestCard (getColumnsHeads (getColumns eoboard))))
            newAceRes = removeRes (getLowestCard (getReserve eoboard)) (addAceToFoundation eoboard (getLowestCard (getReserve eoboard)))
            newSuccRes = removeRes reserveCard (addCardToFoundation eoboard reserveCard)
            newSuccCol = removeCol columnCard (addCardToFoundation eoboard columnCard)
            reserveCard = head (allMovableCard (getFoundationHeads (getFoundation eoboard)) (getReserve eoboard))
            columnCard = head (allMovableCard (getFoundationHeads (getFoundation eoboard)) (getColumnsHeads (getColumns eoboard)))

  {-*********************************************************************************************************

  toFoundations :: parse in an eoboard -> return a new eoboard with as many possible moves made
  toFoundations eoboard (eoboard parsed to function)
      |if column heads of eobaord contain an ace, then add ace card to foundation and remove from column
      |if the reserve deck of eoboard contains an ace, then add ace card to foundation and remove from reserve
      |if the list of all move able cards on eoboard is empty, then autoplay is stopped and the new eoboard is returned 
      |if the reserve deck contains a successor to any of the cards in top of foundation piles, then add card to foundation and remove from reserve
      |if the columns heads contains a successor to any of the cards in top of foundation piles, then add card to foundation and remove from column
      where newAceCol = returns a new eoboard where the ace has been removed from column and added to new pile in foundation
            newAceRes = returns a new eoboard where the ace has been removed from reserve and added to new pile in foundation
            newSuccRes = returns a new eoboard where successor card has been removed from reserve and added to predeccssor card in foundation pile
            newSuccCol = returns a new eoboard where successor card has been removed from column and added to predeccssor card in foundation pile
            reserveCard = is the card in the reserve which can be moved to foundation
            columnCard = is the card at the top of one of the columns which can be moved to foundation

  **********************************************************************************************************-}  

  {-***********************
  --merge
  *************************-}
  my_merge :: Ord a => (a->a->Bool) -> [a] -> [a] -> [a] --takes two lists and returns a merged list of the two
  my_merge compfunc [] list2 = list2 --list1 is empty, return list2
  my_merge compfunc list1 [] = list1 --list2 is empty, return list1
  my_merge compfunc (h1:t1) (h2:t2) --compfunc returns true or false
    | compfunc h1 h2 = (h1:my_merge compfunc t1 (h2:t2)) --if h1>h2, put h1 to head the recurse through rest of list
    | otherwise      = (h2:my_merge compfunc t2 (h1:t1))

  {-***********************
  --mergesort
  *************************-}
  mergesort :: (Ord a, Show a) => (a->a->Bool) -> [a] -> [a] --takes in one unordered list and returns an ordered list
  mergesort compfunc [] = [] --check this only once
  mergesort compfunc dlist =
    (mergesortA compfunc (map (\e -> [e]) dlist)) --take items in list and create sublists. [5, 3, 6] -->> [[5], [3], [6]]

  mergesortA :: (Ord a) => (a->a->Bool) -> [[a]] -> [a] --mergesortA will return a list of length 1, like [[1,2,3,4]]
  mergesortA _ [list] = list
  mergesortA compfunc mlist =  mergesortA compfunc (mergesortpass compfunc mlist) 

  mergesortpass :: Ord a => (a->a->Bool)->[[a]] -> [[a]] --uses my_merge
  mergesortpass _ [] = []
  mergesortpass _ [l] = [l] --one element, return list unchanged
  mergesortpass compfunc (list1:(list2:rest)) = (my_merge compfunc list1 list2): mergesortpass compfunc rest 