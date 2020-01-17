import Prelude
import System.Environment ( getArgs )
import Data.List
import Helpers
  
-- The main method that will be used for testing / command line access
main = do
     args <- getArgs
     filename <- readFile (head args)
     (moves,maze) <- readMazeFile filename
     print "Result"
    -- let indexes = find2D '1' maze
     printMaze (onePlayerManySlides maze moves )

-- YOUR CODE SHOULD COME AFTER THIS POINT
onePlayerManySlides :: [[Char]] -> [Char] -> [[Char]]
onePlayerManySlides maz [] 
      | get2D maz (0,0) == 'g' = (set2D (0,0) 'x' maz) -- Flag to check that a player got to 'goal'  
      | otherwise = maz
onePlayerManySlides maz  _  
      | get2D maz (0,0) == 'g' = (set2D (0,0) 'x' maz) -- Flag to check that a player got to 'goal'  
onePlayerManySlides maz movs  = (onePlayerManySlides newMaze mov )
      where oneMove = head movs
            newMaze = (onePlayerOneMove oneMove maz (head indexes) )
            indexes = find2D '1' maz
            mov = drop 1 movs






