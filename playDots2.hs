-- CPSC 312 Haskell project 2018
module PlayDots2 where


import DotsNboxes2
import System.IO
import Text.Read
import Data.Maybe


-- type Score = (Int, Int) -- myboxes, thyboxes

data Turn = Computer     -- Computer
          | Person       -- Human player
        deriving (Eq, Show)

-- playDots :: Game -> Result -> Player -> Turn -> IO ()

{-
playDots game start opponent turn =
    let ContinueGame(State (played ,(myboxes, thyboxes)) avail size) = start in
    do
        putStrLn "Please input your desired board width: "
        n <- getLine
        let num = read n
        let newStart = ContinueGame (restart num)
        putStrLn ("The Score is: "++" mine: " ++ show myboxes++ "its: " ++ show thyboxes)
        putStrLn "Who starts? Choose 0=you, 1=computer, 2=exit."
        line <- getLine
        if line == "0"
           then
               me_play game newStart opponent Person
           else if line == "1"
           then it_play game newStart opponent Computer
           else if line == "2"
               then return turn
           else playDots game newStart opponent turn
-}

me_play :: Game -> Result -> Player -> Turn -> Bar -> (Result, Turn)
-- opponent has played, the person must now play
me_play game (EndOfGame (-1)  start_state) opponent turn move =
        -- "You won! /n Now play again!"
        ((StartOfGame start_state), turn)

me_play game (EndOfGame 0 start_state) opponent turn move =
        -- "It's a tie"
        ((StartOfGame start_state), turn)

me_play game (EndOfGame 1 start_state) opponent turn move =
        -- Computer won
        ((StartOfGame start_state), turn)

me_play game (ContinueGame state) opponent turn move =
        ((game move state), Computer)


it_play :: Game -> Result -> Player -> Turn -> (Result, Turn)
-- person has played, the computer must now play
it_play game (EndOfGame (-1)  start_state) opponent turn =
        -- "Computer won! /n Now play again!"
        ((StartOfGame start_state), turn)

it_play game (EndOfGame 0 start_state) opponent turn =
        -- "It's a tie"
        ((StartOfGame start_state), turn)

it_play game (EndOfGame 1 start_state) opponent turn =
        -- "You won!"
        ((StartOfGame start_state), turn)

it_play game (ContinueGame state) opponent score =
        -- The computer choses
        ((game opponent_move state), Person)
        where opponent_move = opponent state

simple_opponent (State board (h:t) n) = h
