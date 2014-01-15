myGcd :: Int -> Int -> Int
myGcd a 0 = abs a
myGcd a b = myGcd b k
	where k = a `mod` b
