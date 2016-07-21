extractor :: Integral a => a -> [a] -> [a]
extractor x (y:ys) | isequal x y == True = ys
				 | otherwise = (y:extractor x ys)
				 
isequal :: Integral a => a -> a -> Bool
isequal x y = if x == y then True else False

test :: Integral a => a -> [a] -> [a]
test x y | elem x y == True = extractor x y
		 | otherwise = y