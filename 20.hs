removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (elem, take index xs ++ drop n xs)
	where
		elem = xs !! index
		index = n - 1
