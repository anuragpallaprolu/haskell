divides :: Int -> Int -> Bool
divides d n = rem n d == 0

ldf :: Int -> Int -> Int
ldf k n | divides k n = k
		| k^2 > n 	  = n
		| otherwise   = ldf (k+1) n

ld :: Int -> Int
ld n = ldf 2 n

isPrime n | n < 1     = error "Not a pos int"
		  | n == 1    = False
		  | otherwise = ld n == n

primes :: Int -> [Int]
primes x | x == 1 = []
		 | x < 1 = error "WTAF"
		 | otherwise = [y | y <- [1..x], isPrime y == True]

exclude :: Int -> [Int] -> [Int]
exclude k x = (take (k-1) x) ++ (drop k x)
		 
digsplit :: Integral x => x -> [x]
digsplit 0 = []
digsplit x = (digsplit (div x 10)) ++ [x `mod` 10]

permute :: [Int] -> [[Int]]
permute x | length x == 2 = [[(head x),(last x)],[(last x),(head x)]] 
		  | otherwise = [(x!!(a-1)):b | a <- [1..(length x)], b <- (permute (exclude a x))]

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d
		  
aleph = map digsplit $ filter (\x -> mod x 1000 /= x) (primes 10000)
beth = map permute $ aleph
gamma = [map fromDigits k | k <- beth]
sol x = filter (\(a,b,_) -> (elem (2*b - a) x) && (isPrime (2*b - a)) && (isPrime a) && (isPrime b)) [(i,j,(2*j-i))| i <- x, j <- x, i /= j]
solved = [sol x|x<- gamma] 


