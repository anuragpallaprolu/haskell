
stringToint :: [String] -> [[Double]]
stringToint [] = [[]]
stringToint (x:xs) = [a | a <- map read $ words x :: [Double]]:stringToint xs
