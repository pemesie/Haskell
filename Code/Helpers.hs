module Helpers
( readMazeFile
, printMaze
, get
, get2D
,find
,find2D
,set
,set2D
,onePlayerOneMove
,slidingUp
,slidingDown
,findAllPlayers
,checkPlayers
,quicksort
,allPlayers
,manyPlayerOneMove 
,playerSlidingUp
,playerSlidingDown
,playerSlidingRight
,playerSlidingLeft
,stopMove
) where

import Prelude

readMazeFile :: String -> IO ([Char],[[Char]])
readMazeFile = readIO

printMaze :: [[Char]] -> IO ()
printMaze [] = do print "END"
printMaze (ro:ros) = do 
	                   print ro
	                   printMaze ros

get :: [a] -> Int -> a
get (element:rest) 0 = element
get (element:rest) ind = get rest (ind-1)

get2D :: [[a]] -> (Int,Int) -> a
-- get2D ["alpha","bravo","charlie"] (r,c) -> something
get2D (row:rows) (0,c) = get row c
get2D (row:rows) (r,c) = get2D rows (r-1,c)

find :: Eq a => a -> [a] -> [Int]
find _ [] = []
find e (h:t)
	| e == h = [0] ++ (map (+ 1) (find e t))
	| otherwise = (map (+ 1) (find e t))

find2D :: Eq a => a -> [[a]] -> [(Int,Int)]
-- find2D 'a' ["alpha","bravo","charlie"] equals [(0,0),(0,4),(1,2),(2,2)]
find2D _ [] = []
find2D el (row:rows) = add ++ next
	where 	first = find el row
		add   = [(0,c)|c<-first]
		rest  = (find2D el rows)
		next  = [(r,c)|(a,c)<-rest,let r=a+1]

set :: Int -> a -> [a] -> [a]
-- set 3 'x' "nebraska" equals "nebraska"
set 0 el (h:t) = (el:t)
set n el (h:t) = h:(set (n-1) el t)

set2D :: (Int,Int) -> a -> [[a]] -> [[a]]
-- set2D (1,3) 'w' ["alpha","bravo", "Charly"] equals ["alpha","brawo", "Charly"]
set2D (0,c) el (row:rows) = (set c el row):(rows)
set2D (r,c) el (row:rows) = row:( set2D ((r-1), c) el rows)  

onePlayerOneMove :: Char -> [[Char]] -> (Int, Int) -> [[Char]]
onePlayerOneMove _ [] _ = [] -- base case
onePlayerOneMove m (row:rows) (i,j)  
    | m == 'u' = slidingUp (row:rows) (i-1,j) 
    | m == 'd' = slidingDown (row:rows) (i+1,j)
    | m == 'r' = slidingRight (row:rows) (i,j+1)
    | m == 'l' = slidingLeft (row:rows) (i,j-1)
    | otherwise = (row:rows)
  

slidingUp :: [[Char]] -> (Int, Int) -> [[Char]]
-- slidingUp (row:rows) (0,j) =\ (row:rows) 
slidingUp (row:rows) (i,j) 
    | (get2D (row:rows) (i,j) ) == 'x'= (row:rows)
	| (get2D (row:rows) (i,j) ) == 'g' = set2D (i,j) '1' (set2D (0,0) 'g' newMaze) -- slidingUp (set2D (i,j) '1' newMaze) (i-1,j) 
	| (get2D (row:rows) (i,j) ) /= 'x' = slidingUp (set2D (i,j) '1' newMaze) (i-1,j) 
	where newMaze = (set2D (i+1,j) '-' (row:rows))

slidingDown :: [[Char]] -> (Int, Int) -> [[Char]]
slidingDown (row:rows) (i,j)
    | get2D (row:rows) (i,j) == 'x'= (row:rows)
    | get2D (row:rows) (i,j) == 'g' = set2D (i,j) '1' (set2D (0,0) 'g' newMaze) --slidingDown (set2D (i,j) '1' newMaze) (i+1,j)
    | get2D (row:rows) (i,j) /= 'x' = slidingDown (set2D (i,j) '1' newMaze) (i+1,j)
    where newMaze = (set2D (i-1,j) '-' (row:rows)) 
       

slidingRight :: [[Char]] -> (Int, Int) -> [[Char]]
slidingRight (row:rows) (i,j)
    | (get2D (row:rows) (i,j) ) == 'x'= (row:rows)
    | (get2D (row:rows) (i,j) ) == 'g' = set2D (i,j) '1' (set2D (0,0) 'g' newMaze) --slidingRight (set2D (i,j) '1' newMaze) (i,j+1)
    | (get2D (row:rows) (i,j) ) /= 'x' = slidingRight (set2D (i,j) '1' newMaze) (i,j+1)
    where newMaze = (set2D (i,j-1) '-' (row:rows))         


slidingLeft :: [[Char]] -> (Int, Int) -> [[Char]]
slidingLeft (row:rows) (i,j)
    | (get2D (row:rows) (i,j) ) == 'x'= (row:rows)
    | (get2D (row:rows) (i,j) ) == 'g' = set2D (i,j) '1' (set2D (0,0) 'g' newMaze) -- slidingLeft (set2D (i,j) '1' newMaze) (i,j-1)
    | (get2D (row:rows) (i,j) ) /= 'x' = slidingLeft (set2D (i,j) '1' newMaze) (i,j-1)
    where newMaze = (set2D (i,j+1) '-' (row:rows)) 

-- ********* *********** DEALING WITH MANY PLAYERS ************* **********

-- findAllPlayers :: [[Char]] -> [Char]
-- findAllPlayers [] = []
-- findAllPlayers (row:rows)
--     | '1' `elem` row = ['1'] ++ (findAllPlayers rows) 
--     | '2' `elem` row = ['2'] ++ (findAllPlayers rows) 
--     | '3' `elem` row = ['3'] ++ (findAllPlayers rows) 
--     | '4' `elem` row = ['4'] ++ (findAllPlayers rows)  
--     | otherwise = (findAllPlayers rows)
    -- where
    --     a = '1'
    --     b = '2'
    --     c = '3'
    --     d = '4'

findAllPlayers :: [[Char]] -> [Char]
findAllPlayers [] = []
findAllPlayers (row:rows) = (checkPlayers row) ++ (findAllPlayers rows)

checkPlayers :: [Char] -> [Char]
checkPlayers [] = []
checkPlayers a
    | '1' == h = ['1'] ++ checkPlayers (drop 1 a)
    | '2' == h = ['2'] ++ checkPlayers (drop 1 a)
    | '3' == h = ['3'] ++ checkPlayers (drop 1 a)
    | '4' == h = ['4'] ++ checkPlayers (drop 1 a)
    | otherwise = checkPlayers (drop 1 a)
    where h = head a

-- Get sorted list of players
allPlayers :: [[Char]] -> [Char]
allPlayers (row:rows) = quicksort(findAllPlayers( row:rows )) 

manyPlayerOneMove :: Char -> [[Char]] -> (Int, Int) -> Char -> [[Char]]
manyPlayerOneMove _ [] _ _ = [] -- base case
manyPlayerOneMove m (row:rows) (i,j) player
    | m == 'u' = playerSlidingUp (row:rows) (i-1,j) player
    | m == 'd' = playerSlidingDown (row:rows) (i+1,j) player
    | m == 'r' = playerSlidingRight (row:rows) (i,j+1) player
    | m == 'l' = playerSlidingLeft (row:rows) (i,j-1) player
    | otherwise = (row:rows)

playerSlidingUp :: [[Char]] -> (Int, Int) -> Char ->[[Char]]
playerSlidingUp (row:rows) (i,j) player
    | stopMove (row:rows) (i,j) = (row:rows)
    | (get2D (row:rows) (i,j) ) == 'g' = set2D (i,j) player (set2D (0,0) 'g' newMaze) --playerSlidingUp (set2D (i,j) player (set2D (0,0) 'g' newMaze) ) (i-1,j) player -- the most inner set2D adds a flag to end the game
    | (get2D (row:rows) (i,j) ) /= 'x' = playerSlidingUp (set2D (i,j) player newMaze) (i-1,j) player
    where newMaze = (set2D (i+1,j) '-' (row:rows))



playerSlidingDown :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
playerSlidingDown (row:rows) (i,j) player
    | stopMove (row:rows) (i,j) = (row:rows)
    | (get2D (row:rows) (i,j) ) == 'g' = set2D (i,j) player (set2D (0,0) 'g' newMaze) --playerSlidingDown (set2D (i,j) player (set2D (0,0) 'g' newMaze)) (i+1,j) player -- the most inner set2D adds a flag to end the game
    | (get2D (row:rows) (i,j) ) /= 'x' = playerSlidingDown (set2D (i,j) player newMaze) (i+1,j) player
    where newMaze = (set2D (i-1,j) '-' (row:rows)) 
       

playerSlidingRight :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
playerSlidingRight (row:rows) (i,j) player
    | stopMove (row:rows) (i,j) = (row:rows)
    | (get2D (row:rows) (i,j) ) == 'g' = set2D (i,j) player (set2D (0,0) 'g' newMaze) --playerSlidingRight (set2D (i,j) player (set2D (0,0) 'g' newMaze)) (i,j+1) player -- the most inner set2D adds a flag to end the game
    | (get2D (row:rows) (i,j) ) /= 'x' = playerSlidingRight (set2D (i,j) player newMaze) (i,j+1) player
    where newMaze = (set2D (i,j-1) '-' (row:rows))         



playerSlidingLeft :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
playerSlidingLeft (row:rows) (i,j) player
    | stopMove (row:rows) (i,j) = (row:rows)
    | (get2D (row:rows) (i,j) ) == 'g' = set2D (i,j) player (set2D (0,0) 'g' newMaze) --playerSlidingLeft (set2D (i,j) player (set2D (0,0) 'g' newMaze)) (i,j-1) player -- the most inner set2D adds a flag to end the game
    | (get2D (row:rows) (i,j) ) /= 'x' = playerSlidingLeft (set2D (i,j) player newMaze) (i,j-1) player
    where newMaze = (set2D (i,j+1) '-' (row:rows)) 

stopMove :: [[Char]] -> (Int, Int) -> Bool
stopMove (row:rows) (i,j) 
    | (get2D (row:rows) (i,j) ) == 'x' || (get2D (row:rows) (i,j) ) == '1' || (get2D (row:rows) (i,j) ) == '2' || (get2D (row:rows) (i,j) ) == '3' || (get2D (row:rows) (i,j) ) == '4' = True
    | otherwise = False


-- lengthOfMarix :: [[Char]] -> Int
-- lengthOfMarix [] = 0
-- lengthOfMarix (x:xs) = 1 + (lengthOfMarix xs)

quicksort :: Ord el => [el] -> [el]
quicksort [] = []
quicksort (f:r) = (quicksort [x| x <- r, x<=f]) ++ [f] ++ (quicksort [y|y<-r,y>f])


