lenlist :: [Float] -> Float

lenlist [] = 0
lenlist (x:xs) = 1 + lenlist xs

sumlist :: [Float] -> Float
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

average :: [Float] -> Float
average x = (sumlist x)/(lenlist x)