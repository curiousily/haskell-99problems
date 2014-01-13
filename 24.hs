import System.Random
import Control.Monad
import Data.List

removeAt :: Int -> [a] -> [a]
removeAt n xs = take index xs ++ drop n xs
	where
		index = n - 1

diffSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
diffSelect _ 0 gen = ([], gen)
diffSelect [] _ gen = ([], gen)
diffSelect xs n gen
	| n == (length xs) = (xs, gen)
	| otherwise = diffSelect (removeAt (k + 1) xs) n gen'
		where (k, gen') =
			randomR (0, (length xs) - 1) gen

diffSelectIO :: Int -> Int -> IO [Int]
diffSelectIO n m = getStdRandom $ diffSelect [1..m] n

