range :: Int -> Int -> [Int]
range l r = genNext l (r - l + 1)
	where
		genNext :: Int -> Int -> [Int]
		genNext _ 0 = []
		genNext p n = p : genNext (p + 1) (n - 1)

range' l r = take (r-l+1) $ iterate (+1) l
