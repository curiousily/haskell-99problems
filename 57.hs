data Tree a = Empty | Branch a (Tree a) (Tree a)
	deriving (Show, Eq)

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 l2 && mirror r1 r2
mirror Empty Empty = True
mirror _ _ = False

add :: Ord a => a -> Tree a -> Tree a
add x Empty = Branch x Empty Empty
add x t@(Branch y l r) = case compare x y of
				LT -> Branch y (add x l) r
				GT -> Branch y l (add x r)
				EQ -> t

construct xs = foldl (flip add) Empty xs


