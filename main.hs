module Main where
-- use main to play

import Graphics.Gloss
import PlayDots2
import DotsNboxes2
import Graphics.Gloss.Interface.Pure.Game
import Data.Char
import DisplayGame2

main = play (InWindow "Nice Window" (500, 500) (0, 0)) white 5 (StartOfGame (restart 3), Person) render handleEvent stepOn
