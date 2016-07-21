extractor :: Integral a => a -> [a] -> [a]
extractor x (y:ys) | isequal x y == True = ys
				 | otherwise = (y:extractor x ys)
				 
isequal :: Integral a => a -> a -> Bool
isequal x y = if x == y then True else False

test :: Integral a => a -> [a] -> [a]
test x y | elem x y == True = extractor x y
		 | otherwise = y

minima :: [Int] -> Int
minima [] = error "Empty List"
minima [x] = x
minima (x:y) = min x (minima y)
		 
sortInt :: [Int] -> [Int]
sortInt [] = []
sortInt x = m : (sortInt (test m x)) where m = minima x

divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
		| k^2 > n = n
		| otherwise = ldf (k+1) n
		
ld :: Integer -> Integer
ld n = ldf 2 n

factors :: Integer -> [Integer]
factors n | n < 1 = error "Undefined"
		  | n == 1 = []
		  | otherwise = p : factors (div n p) where p = ld n