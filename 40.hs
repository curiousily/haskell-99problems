{-goldbach :: Int -> (Int, Int)
goldbach n = goldbach' n 1 1

goldbach' :: Int -> Int -> Int -> (Int, Int)
goldbach' n l r
	| n == l + r = (l, r)
	| n > l + r = goldbach' n l (nextPrime r)
	| otherwise = goldbach' n (nextPrime l) 1

nextPrime :: Int -> Int
nextPrime n
	| isPrime (n + 1) = n + 1
	| otherwise = nextPrime (n + 1)

isPrime :: Int -> Bool
isPrime n = isPrime' n 2

isPrime' :: Int -> Int -> Bool
isPrime' 2 _ = True
isPrime' n k
	| (n - 1) == k = True
	| n `mod` k == 0 = False
	| otherwise = isPrime' n (k + 1) -}


goldbach n = head $ 
		filter (\(x, y) -> isPrime x && isPrime y) $
		map (\e -> (e, n - e)) [2, 3..n `div` 2]
	
isPrime n = null $ factors n
factors n = filter (isFactor n) [2..n-1]
isFactor a b = a `mod` b == 0
