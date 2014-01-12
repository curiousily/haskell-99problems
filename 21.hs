insertAt :: a -> [a] -> Int -> [a]
insertAt x xs p = take i xs ++ [x] ++ drop i xs
	where
		i = p - 1
