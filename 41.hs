goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' l r k = filter (\(x, y) -> x > k && y > k) $ goldbachList l r

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList l r =  map (\x -> goldbach x) $ filter even [l..r]

goldbach n = head $ 
		filter (\(x, y) -> isPrime x && isPrime y) $
		map (\e -> (e, n - e)) [2, 3..n `div` 2]
	
isPrime n = null $ factors n
factors n = filter (isFactor n) [2..n-1]
isFactor a b = a `mod` b == 0
