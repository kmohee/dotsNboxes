-- CPSC 312 Haskell project 2018
module PlayDots2 where


import DotsNboxes2
import System.IO
import Text.Read
import Data.Maybe

-- (Result, Turn) can be thought as a world state

data Turn = Computer     -- Computer
          | Person       -- Human player
        deriving (Eq, Show)

-- return the next world given the game, current result, opponent, current turn and a move the player can make 
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

-- return the next world given the game, current result, opponent, current turn 
--        the computer makes the move here. 
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

-- a possible opponent of the game
simple_opponent (State board (h:t) n) = h
