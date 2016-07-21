dot :: [Float] -> [Float] -> Float
dot [] [] = 0.0
dot (x:xs) (y:ys) = x*y + dot xs ys

pol_Construct :: Float -> Int-> [Float]
pol_Construct x 0 = [1]
pol_Construct x m = (x^m):pol_Construct x (m-1)