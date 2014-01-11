dupli :: [a] -> [a]
dupli = concatMap duplicator
	where
		duplicator x = replicate 2 x
