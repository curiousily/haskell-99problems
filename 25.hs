import System.Random
import Data.List


rndElem :: [a] -> IO a
rndElem xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index
 
rndPerm :: [a] -> IO [a]
rndPerm xs = rndElem . permutations $ xs
