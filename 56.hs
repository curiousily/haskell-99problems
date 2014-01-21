data Tree a = Empty | Branch a (Tree a) (Tree a)
	deriving (Show, Eq)

leaf x = Branch x Empty Empty

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 l2 && mirror r1 r2
mirror Empty Empty = True
mirror _ _ = False
