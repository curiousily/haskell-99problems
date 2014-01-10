data ListItem a = Single a | Multiple Int a 
	deriving (Show)

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encoder ((length(takeWhile (==x) xs) + 1), x) : encodeDirect (dropWhile (==x) xs)
	where
		encoder (1, x) = Single x
		encoder (n, x) = Multiple n x
