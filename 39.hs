primesR :: Int -> Int -> [Int]
primesR l r 
	| p < r = p : primesR p r
	| otherwise = []
		where
			p = nextPrime l

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
	| otherwise = isPrime' n (k + 1)
