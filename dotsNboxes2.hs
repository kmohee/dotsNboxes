-- CPSC 312 Haskell project 2018
module DotsNboxes2 where
--import Graphics.Gloss

type Dot = (Int, Int)

data Bar = Bar Dot Dot
        deriving (Eq, Show)

type Board = (([Bar], [Bar]), (Int, Int))

data State = State Board [Bar] Int
        deriving (Eq, Show)

data Result = EndOfGame Double State
            | ContinueGame State
            | StartOfGame State
        deriving (Eq, Show)

type Game = Bar -> State -> Result

type Player = State -> Bar

-- The dotsNboxes Game --

-- Game:
-- given a board a state and dimension of nxn board return:
--      EndOfGame 1 if current player won
--      EndOfGame 0 if tie
--      EndOfGame -1 if opponent won
--      ContinueGame nextstate if moves are still available
dNb :: Game
dNb input s1
    | available == [input]  =  gameOver input s1
    | otherwise             =  gameOn input s1
  where State board available n = s1

--given a move, state, and board dimension return ContinueGame with new state
--                      where players lines and current scores are switched,
--                            and move is removed from available moves list
gameOn:: Bar->State->Result
gameOn move (State ((mylines, others),(myboxes, thyboxes)) available n) =
  ContinueGame(State ((others, updatedmoves),(thyboxes,updatedboxes))[x | x <- available, x /=move] n)
    where updatedboxes=myboxes + checkBox move (mylines++others) n
          updatedmoves= move:mylines

-- given a move,state, and board dimensions returns:
--      EndOfGame 1 if current player won (if current player has more boxes than opponent)
--      EndOfGame 0 if tie (if current player has same boxes as opponent)
--      EndOfGame -1 if opponent won (if current player has less boxes than opponent)
-- EndOfGame 1 if current player won, EndOfGame 0 if tie, and EndOfGame -1 if opponent won
gameOver :: Bar -> State -> Result
gameOver move (State ((mylines, others),(myboxes, thyboxes)) available n)
    | boxes > thyboxes = EndOfGame 1 (State ((mylines, others),(myboxes, thyboxes)) available n)
    | boxes == thyboxes = EndOfGame 0 (State ((mylines, others),(myboxes, thyboxes)) available n)
    | otherwise = EndOfGame (-1) (State ((mylines, others),(myboxes, thyboxes)) available n)
  where boxes = myboxes + checkBox move (mylines++others) n  -- counts number of boxes of current player

-- returns number of boxes made by input action, given current board
checkBox :: Bar -> [Bar] -> Int ->Int
checkBox (Bar (x,y) (a,b)) played n = foldr (\x y -> if x == True then y+1 else y) 0 lst
      where lst = [ and [d `elem` played |d <- s] |s <- helper (Bar (x,y) (a,b)) n]

--returns list of possible boxes formed by given line
helper :: Bar -> Int->[[Bar]]
helper (Bar (x,y) (a,b)) n
    | y == b = cleaner1 n [[Bar (x, y-1) (x, y), Bar (x,y-1) (a,b-1), Bar (a,b-1) (a,b)], [Bar (x, y) (x, y+1), Bar (x,y+1) (a,b+1), Bar (a,b) (a,b+1)]]       -- horizontal
    | otherwise = cleaner1 n [[Bar (x-1, y) (x, y), Bar (x-1,y) (a-1,b), Bar (a-1,b) (a,b)], [Bar (x, y) (x+1, y), Bar (x+1,y) (a+1,b), Bar (a,b) (a+1,b)]]    -- vertical

--given list of boxes, removes boxes that lie outside of board
cleaner1 :: Int-> [[Bar]] -> [[Bar]]
cleaner1 n lst = [y|y<-[cleaner n x | x <- lst], y/=[]]

--return [] if given box is out of board bounds, otherwise returns box
cleaner :: Int->[Bar] -> [Bar]
cleaner n lst = [Bar (m,p) (x,y)|Bar (m,p) (x,y) <- lst, m>=0, m<=n, p>=0, p<=n,x>=0, x<=n,y>=0, y<=n]

-- starting state
restart:: Int->State
restart n = State (([],[]),(0,0)) (generateLines n) n

--generates possible moves given board of nxn dimensions
generateLines :: Int -> [Bar]
generateLines n = horizontalLines n ++ verticalLines n

--generates horizontal lines of board of nxn dimensions
horizontalLines :: Int -> [Bar]
horizontalLines n = [Bar (y-1,x) (y,x)|y<-[1..n], x<-[0..n]]

--generates vertical lines of board of nxn dimensions
verticalLines :: Int -> [Bar]
verticalLines n = [Bar(x,y-1) (x,y)|x<-[0..n], y<-[1..n]]


--Test dummies
testLine = (Bar (1,2) (1,3))
testState = State (([Bar (0,2) (1,2),Bar (0,2) (0,3)],[Bar (0,3) (1,3)]),(0,0)) [] 3
testState2 = State (([Bar (0,2) (1,2),Bar (0,2) (0,3),Bar (0,3) (1,3)],[]),(0,0)) [] 3-- one of boxes from testLine in 3x3 grid
testState3 = State (([Bar (0,2) (1,2),Bar (0,2) (0,3),Bar (0,3) (1,3)],[]),(0,0)) [testLine] 3--one move left dummy

--Continue game tests where square constructed by same player lines
--dNb (Bar (1,2)(1,3)) testState2
--Continue game tests where square constructed by both player lines
 --dNb (Bar (1,2)(1,3)) testState
--EndGameDummy current player wins
--dNb testLine testState3
--EndGameDummy current player looses

-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
