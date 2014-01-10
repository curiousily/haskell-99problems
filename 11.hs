data ListItem a = Single a | Multiple Int a 
	deriving (Show)

encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = ((length(takeWhile (==x) xs)) + 1, x) : encode (dropWhile (==x) xs)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encoder . encode
	where
		encoder (1, x) = Single x
		encoder (n, x) = Multiple n x
