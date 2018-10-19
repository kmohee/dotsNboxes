module Opponent where

import DotsNboxes2

smart_opponent (State ((lst1,lst2),score) avail n) = provideH (lst1 ++ lst2) avail n

provideH:: [Bar] -> [Bar] -> Int -> Bar
provideH lst1 (h:t) n
    | chooseH h lst1 n== (0,0) = h
    | chooseH h lst1 n== (0,1) = h
    | chooseH h lst1 n== (1,0) = h
    | chooseH h lst1 n== (1,1) = h
    | chooseH h lst1 n== (0,3) = h
    | chooseH h lst1 n== (3,0) = h
    | chooseH h lst1 n== (3,3) = h
    | otherwise = provideH lst1 t n

generateTop (Bar (x,y) (a,b)) n = [ Bar(a1,b1)(a2,b2)| Bar(a1,b1)(a2,b2) <- [Bar(x,y-1) (x,y), Bar (x,y-1) (a,b-1), Bar (a,b-1) (a,b)], a1>= 0, a1 <= n, a2>= 0, a2 <= n, b1>= 0, b1 <= n, b2>= 0, b2 <= n ]
generateBottom (Bar (x,y) (a,b)) n = [ Bar(a1,b1)(a2,b2)| Bar(a1,b1)(a2,b2) <- [Bar (x,y) (x,y+1), Bar (x,y+1) (a,b+1), Bar (a,b) (a,b+1)], a1>= 0, a1 <= n, a2>= 0, a2 <= n, b1>= 0, b1 <= n, b2>= 0, b2 <= n ]
generateLeft (Bar (x,y) (a,b)) n = [ Bar(a1,b1)(a2,b2)| Bar(a1,b1)(a2,b2) <- [Bar (x-1,y) (x,y), Bar (x-1,y) (a-1,b), Bar (a-1,b) (a,b)], a1>= 0, a1 <= n, a2>= 0, a2 <= n, b1>= 0, b1 <= n, b2>= 0, b2 <= n ]
generateRight (Bar (x,y) (a,b)) n = [ Bar(a1,b1)(a2,b2)| Bar(a1,b1)(a2,b2) <- [Bar (x,y) (x+1,y), Bar (x+1,y) (a+1,b), Bar (a,b) (a+1,b)], a1>= 0, a1 <= n, a2>= 0, a2 <= n, b1>= 0, b1 <= n, b2>= 0, b2 <= n ]

chooseH:: Bar -> [Bar] -> Int -> (Int, Int)
chooseH h lst1 n
    | horizontal h = (countLine (generateTop h n) lst1, countLine (generateBottom h n) lst1)
    | otherwise = (countLine (generateLeft h n) lst1, countLine (generateRight h n) lst1)

countLine:: [Bar] -> [Bar] -> Int
countLine [] lst1 = 0
countLine lst lst1 =  foldr (\x y -> if x == True then y+1 else y) 0 [x `elem` lst1 | x <- lst]

horizontal (Bar(x,y)(a,b))
    | y == b = True
    |otherwise = False
