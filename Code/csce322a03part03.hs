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
     -- print (findAllPlayers maze)
     -- print players
     -- print moves
     print "Result"
     printMaze (manyPlayersOneSlide maze moves players)
  

-- YOUR CODE SHOULD COME AFTER THIS POINT
manyPlayersOneSlide :: [[Char]] -> [Char] -> [Char] -> [[Char]]
manyPlayersOneSlide maz _ []
      | get2D maz (0,0) == 'g' = (set2D (0,0) 'x' maz) -- Flag to check that a player got to 'goal' 
      | otherwise = maz
manyPlayersOneSlide maz [] _ 
      | get2D maz (0,0) == 'g' = (set2D (0,0) 'x' maz) -- Flag to check that a player got to 'goal'  
      | otherwise = maz
manyPlayersOneSlide maz _ _
      | get2D maz (0,0) == 'g' = (set2D (0,0) 'x' maz) -- Flag to check that a player got to 'goal' 
manyPlayersOneSlide maz moveList playersList0 = (manyPlayersOneSlide newMaze move playersList1)
      where oneMove = head moveList
            player = (head playersList0)
            indexes = find2D  player maz
            newMaze = (manyPlayerOneMove oneMove maz (head indexes) player ) 
            move = drop 1 moveList
            playersList1 = drop 1 playersList0
