-- CPSC 312 Haskell project 2018
module DisplayGame2 where


import Graphics.Gloss
import PlayDots2
import DotsNboxes2
import Graphics.Gloss.Interface.Pure.Game
import Data.Char

render :: (Result, Turn) -> Picture
render (ContinueGame state, turn) =
   pictures (renderBoard state ++ renderLines state turn)

render (EndOfGame z state, Person)
    | z == 1 = specialRender (EndOfGame z state, Person) "Computer Won"
    | z == 0 = specialRender (EndOfGame z state, Person) "It's a tie"
    | otherwise = specialRender (EndOfGame z state, Person) "You won"

render (EndOfGame z state, Computer)
    | z == 1 = specialRender (EndOfGame z state, Person) "You won"
    | z == 0 = specialRender (EndOfGame z state, Person) "It's a tie"
    | otherwise = specialRender (EndOfGame z state, Person) "Computer Won"

render (StartOfGame state, __)= scale 0.1 0.1 (pictures[translate 500 300 (Text "Let's Play!"), translate 500 0 (Text "Please press a number from 1-9")])


renderBoard :: State -> [Picture]
renderBoard (State board avail n) =
    [translate x y (Circle 0.3) | (x,y) <- (generateDots n)]

generateDots :: Num a => Int -> [(a, a)]
generateDots n = [(fromIntegral x*50, fromIntegral y*50)|x<-[0..n], y<-[0..n]]

giveState :: Result -> State
giveState (ContinueGame state) = state
giveState (EndOfGame _ state) = state


renderLines :: State -> Turn -> [Picture]
renderLines (State ((lst1,lst2),boxes) avail n) turn
    | turn == Computer =  [color r (Line [(fromIntegral a1*50, fromIntegral b1*50),(fromIntegral a2*50, fromIntegral b2*50)]) | Bar(a1,b1) (a2,b2) <- lst1]
      ++  [color b (Line [(fromIntegral a1*50, fromIntegral b1*50),(fromIntegral a2*50, fromIntegral b2*50)]) | Bar(a1,b1) (a2,b2) <- lst2]
    | otherwise = [color b (Line [(fromIntegral a1*50, fromIntegral b1*50),(fromIntegral a2*50, fromIntegral b2*50)]) | Bar(a1,b1) (a2,b2) <- lst1]
      ++  [color r (Line [(fromIntegral a1*50, fromIntegral b1*50),(fromIntegral a2*50, fromIntegral b2*50)]) | Bar(a1,b1) (a2,b2) <- lst2]
      where b = makeColor 1.0 0.0 0.0 1.0
            r = makeColor 0.0 0.0 1.0 1.0


handleEvent:: Event -> (Result, Turn) -> (Result, Turn)


handleEvent (EventKey (Char c) Down _ (x,y)) (StartOfGame state, Person)
    |n `elem` [1..9] = (ContinueGame (restart n), Person)
    |otherwise = (StartOfGame state,Person)
    where n = (digitToInt c)

handleEvent _ (StartOfGame state, Person)= (StartOfGame state,Person)

-- handleEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) (StartOfGame (State b avail n), Person)= (StartOfGame (restart n),Person)

handleEvent (EventKey (MouseButton LeftButton) Up _ (x,y)) (result, Person)
    |move `elem` avail = me_play dNb result simple_opponent Person move
    |otherwise = (result,Person)
    where move = (findBar(x,y))
          State b avail n = giveState result

handleEvent _ (result, turn) = (result, turn)

stepOn:: Float -> (Result, Turn) -> (Result, Turn)
stepOn iterationTime (result, Person) = (result, Person)
stepOn iterationTime (result, Computer) = it_play dNb result simple_opponent Computer

specialRender:: (Result, Turn) -> [Char] -> Picture
specialRender (result, turn) stringInput =
    pictures (renderBoard (giveState result) ++ renderLines (giveState result) turn ++ imageText stringInput)


imageText :: [Char] -> [Picture]
imageText str = [scale 0.1 0.1 (translate 500 300 (Text str))]

findBar:: (Float, Float) -> Bar
findBar (x,y)
    | unscaledx-(fromIntegral lbx) >= unscaledy-(fromIntegral lby) -- horizontal
      = Bar(lbx, lby) (lbx + 1, lby)
    | otherwise -- vertical
      = Bar(lbx, lby) (lbx, lby + 1)
       where lbx = (floor (unscaledx))::Int
             lby = (floor (unscaledy))::Int
             unscaledx = x/50
             unscaledy = y/50

renderScore:: (Result, Turn) -> Picture
renderScore (result, Person)=
      scale 0.1 0.1
        (pictures
          [translate 500 300 (Text ("Score:")),
             translate 500 600 (color r (Text (show myboxes))), color b (Text (show thyboxes ))])
      where b = makeColor 1.0 0.0 0.0 1.0
            r = makeColor 0.0 0.0 1.0 1.0
            State (lines,(myboxes,thyboxes)) avail n = giveState(result)

renderScore (result, Computer)=
      scale 0.1 0.1
      (pictures
        [translate 500 300 (Text ("Score:")),
           translate 500 600 (color r (Text (show thyboxes))), color b (Text (show myboxes))])
      where b = makeColor 1.0 0.0 0.0 1.0
            r = makeColor 0.0 0.0 1.0 1.0
            State (lines,(myboxes,thyboxes)) avail n = giveState(result)
