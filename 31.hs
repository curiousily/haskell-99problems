isPrime :: Int -> Bool
isPrime n = isPrime' n 2

isPrime' :: Int -> Int -> Bool
isPrime' 2 _ = True
isPrime' n k
	| (n - 1) == k = True
	| n `mod` k == 0 = False
	| otherwise = isPrime' n (k + 1)
