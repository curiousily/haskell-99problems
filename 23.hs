import System.Random
import Control.Monad

rndSelect xs n = replicateM n randomNumber
	where randomNumber = do r <- randomRIO (0, (length xs) - 1)
				return (xs !! r)
