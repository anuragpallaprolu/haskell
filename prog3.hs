divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
		| k^2 > n 	  = n
		| otherwise   = ldf (k+1) n

ld :: Integer -> Integer
ld n = ldf 2 n

isPrime n | n < 1     = error "Not a pos int"
		  | n == 1    = False
		  | otherwise = ld n == n

primes :: Integer -> [Integer]
primes x | x == 1 = []
		 | x < 1 = error "WTAF"
		 | otherwise = [y | y <- [1..x], isPrime y == True]
		 