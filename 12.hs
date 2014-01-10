data ListItem a = Single a | Multiple Int a 
	deriving (Show)

decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified = concatMap toList
	where
		toList (Single a) = [a] 
		toList (Multiple n a) = replicate n a
