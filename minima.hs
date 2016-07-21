minima :: [Int] -> Int
minima [] = error "Empty List"
minima [x] = x
minima (x:y) = min x (minima y)