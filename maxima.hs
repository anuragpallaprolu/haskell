maxima :: (Ord a) => [a] -> a

maxima [] = error "Empty List"
maxima [x] = x
maxima (x:xs) | x > flag = x | otherwise = flag where flag = maxima xs