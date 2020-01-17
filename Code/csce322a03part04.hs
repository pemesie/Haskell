import Prelude
import System.Environment ( getArgs )
import Data.List
import Data.Char
import Helpers
  
-- The main method that will be used for testing / command line access
main = do
     args <- getArgs
     filename <- readFile (head args)
     (moves,maze) <- readMazeFile filename
     let players = allPlayers maze -- return the sorted list of players
     print "Result"
     printMaze (manyPlayersManySlides maze moves players)

-- YOUR CODE SHOULD COME AFTER THIS POINT
-- manyPlayersManySlides :: [[Char]] -> [Char] -> [[Char]]
-- manyPlayersManySlides ma mos = ma

     
    

-- YOUR CODE SHOULD COME AFTER THIS POINT
manyPlayersManySlides :: [[Char]] -> [Char] -> [Char] -> [[Char]]
-- manyPlayersManySlides maz _ []
-- 	  | get2D maz (0,0) == 'g' = (set2D (0,0) 'x' maz) -- Flag to check that a player got to 'goal' 
manyPlayersManySlides maz [] _ 
      | get2D maz (0,0) == 'g' = (set2D (0,0) 'x' maz) -- Flag to check that a player got to 'goal' 
      | otherwise = maz
manyPlayersManySlides maz _ _
      | get2D maz (0,0) == 'g' = (set2D (0,0) 'x' maz) -- Flag to check that a player got to 'goal' 
manyPlayersManySlides maz (m:ms) (p:ps) = (manyPlayersManySlides newMaze move playersList1)
      where oneMove = m --get head
            index = head(find2D p maz) -- find index of player p
            newMaze = (manyPlayerOneMove oneMove maz index p ) 
            move = ms --drop first elt) 
            playersList1 = (addToList p ps) -- This put the head of the list in the back of the list

addToList :: Char -> [Char] -> [Char]
addToList a [] = [a]
addToList a (row:rows) = row: addToList a rows
