{-***********************************
EOIO.hs
Programe      : Eight-Off Display
Author        : James Milton 
last modified : --/12/15
************************************-}
module EOIO where

  import EOSolitaire
  import Data.Maybe

  --displayEOB
  displayEOB :: EOBoard -> IO String
  displayEOB (foundations,columns,reserve) = do
    let columnsString = colsToString columns
    putStr "EOBoard\nFoundations  "
    putStrLn (show foundations)
    putStr  "Columns"
    putStr columnsString
    putStr "\n\nReserve      "
    putStrLn (show reserve)
    return ""

  --colsToString
  colsToString :: Columns->String 
  colsToString columns = foldr (++) "" ["\n             "++(show col) |col<-columns]


  --displayEOBList
  displayEOBList :: [EOBoard]-> IO String
  displayEOBList eoboard 
      | null eoboard = (return "")
      | otherwise = do displayEOB (head eoboard)
                       displayEOBList (tail eoboard)


  --scoreBoard (number cards in foundation)
  scoreBoard :: EOBoard -> String 
  scoreBoard (foundations, columns, reserve) = (show (52- (length reserve) - (foldr (+) 0 (map length columns))))      

  --displayEOGame
  displayEOGame :: EOBoard ->IO String
  displayEOGame eoboard = do
   let (fnds,cols,res) = eoboard
   if ((null cols)&&(null res)) 
      then return "A WIN"
      else 
       do
        displayEOB eoboard
        let res = chooseMove eoboard
        if (isJust res) then
                do
                 let nb = resMaybe res
                 displayEOGame nb
               else
                do
                  let score = scoreBoard eoboard
                  return score



  