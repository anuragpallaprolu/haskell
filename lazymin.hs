lazymin :: [Int] ->  Int -> Int
lazymin (x:xs) current_min | xs == []        = if x < current_min then x else current_min
						   | x < current_min = lazymin xs x
						   | otherwise       = lazymin xs current_min
						   