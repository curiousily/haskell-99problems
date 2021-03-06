import Control.Monad

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
infixl 3 `equ'`

tablen :: Int -> ([Bool] -> Bool) -> IO()
tablen n f = mapM_ putStrLn [toStr x ++ " " ++ show (f x)|x <- xs]
	where 
		xs = replicateM n [True, False]
		toStr = unwords . map (\x -> show x ++ " ")
