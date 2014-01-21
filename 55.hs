data Tree a = Empty | Branch a (Tree a) (Tree a)
	deriving (Show, Eq)

leaf x = Branch x Empty Empty

consBalanceTree 0 = [Empty]
consBalanceTree n = let (q, r) = (n - 1) `quotRem` 2
	in [Branch 'x' left right | i <- [q.. q + r],
				left <- consBalanceTree i,
				right <- consBalanceTree (n - i - 1)]
