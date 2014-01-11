dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery (x:xs) 1 = dropEvery xs 3
dropEvery (x:xs) n = x : dropEvery xs (n - 1)
