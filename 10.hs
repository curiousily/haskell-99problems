encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = ((length(takeWhile (==x) xs)) + 1, x) : encode (dropWhile (==x) xs)
