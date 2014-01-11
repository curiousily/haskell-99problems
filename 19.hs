rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n
	| n > 0 = drop n xs ++ take n xs
	| n < 0 = drop tailSize xs ++ take tailSize xs
		where
			tailSize = (length xs) + n
