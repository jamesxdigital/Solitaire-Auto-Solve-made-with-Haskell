---
layout: post
title: Lovely Functional Haskell
image: /images/haskell/findmoves1.png
---

For an assignment, I had to make an implementation of a Solitaire (Patience) card game in Haskell, an advanced, purely functional programming language.  The particular variant of Solitaire is called [8-Off](https://en.wikipedia.org/wiki/Eight_Off).

The main bulk of this page is showing how I made an 'auto-solve' function, which when given a deck of cards, will attempt to solve the game itself whilst considering future moves.

**TL;DR:** A professional Eight Off Solitaire player will win a game (correctly place all 52 cards) 33.3% of the time. My Haskell 'auto-solve' implementation won a game 29.4% of the time.


---

#### Design

![](http://jamesmilton.me/images/haskell/design.png)

---

#### Functions

#####  `moveKingToEmptyColumn :: EOBoard -> EOBoard`
Given an EOBoard, if there is an empty Column and there is a King at the head of one of the Columns, then this function will return an EOBoard where the King has been moved to the empty Column.

#####  `moveKingResToEmptyColumn :: EOBoard -> EOBoard`
Given an EOBoard, if there is an empty Column and there is a King in the reserve, then this function will return a EOBoard where the King has been moved to the empty Column.

#####  `moveResPredColHead :: EOBoard -> EOBoard`
Given an EOBoard, if the reserve contains a card which is the predecessor of one of the column heads, this function will then return an EOBoard where that card has then been moved onto of the card in the columns.

#####  `moveColsSucc :: EOBoard -> EOBoard`
Given an EOBoard, where in the Columns, there are cards at the head of two of the columns that are successors, this function will return an EOBoard where the column head predecessor has been moved onto the column where it’s head card is the successor of that card.

#####  `move2ndKingRes :: EOBoard -> EOBoard`
Given an EOBoard, if the columns contain a column which 2nd from top card is a king and there is an empty column and there is space in the reserve, this function will then return an EOBoard where that columns head card is moved to the reserve, consequently exposing the King.

#####  `move2ndColSucc :: EOBoard -> EOBoard`
Given an EOBoard, if second card in a column is a successor of another column head, move first card of the original column to reserve.

#####  `moveSingleColRes :: EOBoard -> EOBoard`
Given an EOBoard, if there contains a column which is of only length 1 and there is space in the reserve and the card is not a King, this function will return an EOBoard .where that card is then moved to the reserve, giving one extra empty column.
moveNthCardHeadRes :: EOBoard -> Int -> EOBoard
Given an EOBoard, if Nth card in column can be moved to foundation, this function will return an EOBoard where the head of the column containing the nth card is moved to the reserve.

#####  `findMoves :: EOBoard -> [EOBoard]`
Given an EOBoard, findMoves will display the outputs of the above functions, if legal. If the function move is legal and not the same as the given EOBoard, it will be added to the list findMoves outputs. If the move is not available or it is not different to the original EOBoard, it will not be shown on the in the outputted list. findMoves outputs the list of EOBoards in order of how good of a move they are. For example, an EOBoard at the top of the list from findMoves will be better than an EOBoard move at the bottom of the list.
![](http://jamesmilton.me/images/haskell/findmoves1.png)

![](http://jamesmilton.me/images/haskell/findmoves2.png)

![](http://jamesmilton.me/images/haskell/findmoves3.png)

![](http://jamesmilton.me/images/haskell/findmoves4.png)

![](http://jamesmilton.me/images/haskell/findmoves5.png)

#####  `chooseMove :: EOBoard -> Maybe EOBoard`
Given an EOBoard, chooseMove takes that EOBoard and then passes it to findMoves to find the list of available moves. If findMoves returns with a list of no EOBoards, chooseMove will consquesntly return Nothing. If findMoves returns with a list of at least 1 EOBoard, chooseMoves will return Just the first EOBoard in the list, as that would be the best move in the list.
![](http://jamesmilton.me/images/haskell/choosemoves1.png)

![](http://jamesmilton.me/images/haskell/choosemoves2.png)

![](http://jamesmilton.me/images/haskell/choosemoves3.png)

#####  `score :: EOBoard -> Int`
Given an EOBoard, score will return an Int corresponding to the number of cards that have been moved to the foundation. A winning score would be a score of 52.
![](http://jamesmilton.me/images/haskell/score1.png)

![](http://jamesmilton.me/images/haskell/score2.png)

![](http://jamesmilton.me/images/haskell/score3.png)

![](http://jamesmilton.me/images/haskell/score4.png)

#####  `eOGame :: EOBoard -> Int`
Given an EOBoard, eOGame will play that EOBoard and will then return the score of which it managed to get to by exhausting all available moves.
![](http://jamesmilton.me/images/haskell/eogame1.png)

![](http://jamesmilton.me/images/haskell/eogame2.png)

![](http://jamesmilton.me/images/haskell/eogame3.png)

#####  `eOExptAUX :: Int -> [Int]`
Given an initial random seed, eOExpt will pass the initial random seed to eODeal and pass the score from that eODeal game into a list. eODeal will then increment that seed 100 times and then return a list of the 100 scores.
average :: (Real a, Fractional b) => [a] -> b
Given a list of Ints, average will simply find the average of that list and then return as a float.
![](http://jamesmilton.me/images/haskell/eoexptaux1.png)

![](http://jamesmilton.me/images/haskell/eoexptaux2.png)

#####  `eOExpt :: Int -> (Int,Float)`
Given an initial random seed, eOExpt will pass the initial random seed to eOExptAUX which will then return a list of 100 scores from the initial seed. This list is then processed by average to return the average of all the scores. As well as this, the list from eOExptAUX is then filtered to find the number of times there was a win. Both the number of wins and the average of all the score are then returned as a tuple.
![](http://jamesmilton.me/images/haskell/eoexpt1.png)

![](http://jamesmilton.me/images/haskell/eoexpt2.png)

---

#### Experimentation and Results

Given an initial random seed, eOExpt will pass the initial random seed to eOExptAUX which will then return a list of 100 scores from the initial seed. This list is then processed by average to return the average of all the scores. As well as
this, the list from eOExptAUX is then filtered to then find the number of times there was a win. Both the number of wins and the average of all the scores are then return as a tuple.

|   eOExpt 2  |
|:-----------:|
|  `(20,16.86)` |


In this example above with an initial seed of 2, out 100 tests, 20 of them got a score of 52 (i.e. a success rate of 20%) and the average score of them all was 16.86. Below is 24 random examples of eOExpt:


|  **eOExpt 500** |  **eOExpt 600** |  **eOExpt 700** |  **eOExpt 800** |  **eOExpt 900** |
|  `(31,22.36)` |  `(32,22.21)` |  `(22,19.05)` |  `(29,22.27)` |  `(30,21.68)` |
| **eOExpt 1100** | **eOExpt 1200** | **eOExpt 1300** | **eOExpt 1400** | **eOExpt 1500** |
|  `(24,19.37)` |  `(32,22.07)` |  `(24,18.85)` |  `(28,21.05)` |  `(28,21.05)` |
| **eOExpt 1600** | **eOExpt 1700** | **eOExpt 1800** | **eOExpt 1900** | **eOExpt 100** |
|  `(31,23.41)` |  `(30,21.58)` |  `(28,21.11)` |  `(29,22.2)`  |  `(28,20.75)` |
| **eOExpt 2100** | **eOExpt 2400** | **eOExpt 3200** | **eOExpt 3300** | **eOExpt 3400** | 
|  `(30,22.38)` |  `(34,23.51)` |  `(37,24.81)` |  `(30,20.57)` |  `(31,22.13)` |



|              List of number of wins from table:             | Total of wins from 2400 games|
|:-----------------------------------------------------------:|:-:|
| `31,32,22,29,30,24,32,24,28,28,31,30,28,29,28,30,34,37,30,31` | 588|



|        **Mean**        |    `29.4`   |
|      **Variance**      | `11.140001` |
| **Standard deviation** | `3.3376641` |

---
#### Summary

From the results, it is clear from the fairly large variance that a score can vary from being 52 or getting to about half way and then getting stuck. 

What I think this may be from is that some moves that are being chosen may actually be causing the the cards that need to go to the foundation to be pushed further down the column. 

Also, because the move moveNthCardHeadRes is the most favored move after toFoundations, if the next available card was an ACE, 5 cards back from the top of the column, this move will be favored over maybe putting a KING in an empty column, which could cause separation of more cards, making more foundation movable cards visible.

If I wasn’t limited for time, this is something I could of implemented and consequently could have maybe yielded a better number of wins. 

However, overall average of 24 tests of eOExpt of 100 games, (i.e. 2400 games) the mean percentage of scores of 52 was 29.4%, which I think is a very respectable score, seeing as professional Eight Off players win 33.3% of the time. 
