import Prelude
import System.Environment ( getArgs )
import Data.List
import Helpers
import System.IO

-- The main method that will be used for testing / command line access
main = do
    args <- getArgs
    filename <- readFile (head args)
    (moves,maze) <- readMazeFile filename
    print "Result"
    let indexes = find2D '1' maze
   -- print moves
--    print (lengthOfMarix maze)
    printMaze (onePlayerOneSlide maze (head moves) (head indexes) )
 
-- YOUR CODE SHOULD COME AFTER THIS POINT
onePlayerOneSlide :: [[Char]] -> Char -> (Int, Int) ->[[Char]]
-- onePlayerOneSlide maz mov index  onePlayerOneMove mov maz index
onePlayerOneSlide maz mov index 
   | get2D ma (0,0) == 'g' = (set2D (0,0) 'x' ma) -- Flag to check that a player got to 'goal'  
   | otherwise = ma
   where ma =  onePlayerOneMove mov maz index

