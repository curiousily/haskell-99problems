
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True True = True
or' True _ = True
or' _ False = True
or' _ _ = False

xor' :: Bool -> Bool -> Bool
xor' True _ = True
xor' _ False = True
xor' _ _ = False

nand' :: Bool -> Bool -> Bool
nand' x y = not' $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not' $ or' x y

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' a b = not' $ xor' a b 

infixl 4 `or'`
infixl 6 `and'`

table' :: (Bool -> Bool -> Bool) -> IO()
table' f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
					| a <- [True, False], b <- [True, False]] 
