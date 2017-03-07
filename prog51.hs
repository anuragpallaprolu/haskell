import System.Random
import System.IO.Unsafe
import Data.Function (on)
import Data.List (sortBy, group, sort)
import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

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

replace :: Int -> Int -> [Int] -> [Int]
replace i n x = (take (i-1) x) ++ [n] ++ (drop i x) 

replace_l :: [Int] -> Int -> [Int] -> [Int]
replace_l a n x | length a == 1 = replace (head a) n x
			    | otherwise     = replace_l (tail a) n (replace (head a) n x)
		 
digsplit :: Integral x => x -> [x]
digsplit 0 = []
digsplit x = (digsplit (div x 10)) ++ [x `mod` 10]

permute :: [Int] -> [[Int]]
permute x | length x == 2 = [[(head x),(last x)],[(last x),(head x)]] 
		  | otherwise = [(x!!(a-1)):b | a <- [1..(length x)], b <- (permute (exclude a x))]

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0 where addDigit num d = 10*num + d

sublistofsize 0 _        = [[]]
sublistofsize _ []       = []
sublistofsize n (x : xs) = sublistsThatStartWithX ++ sublistsThatDontStartWithX
  where sublistsThatStartWithX = map (x:) $ sublistofsize (n-1) xs
        sublistsThatDontStartWithX = sublistofsize n xs

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)
		
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

replace_all_occ :: Int -> Int -> [Int] -> [Int]
replace_all_occ i j x | count i x == 0 = x
					  | otherwise      = [if tx == i then j else tx|tx <- x] 

freq l = filter (\(_,b) -> b > 1) [(k,count k l)|k<-(rmdups l)]

-------------------


					  
prime_list = filter (\x -> isPrime x) [x | x <- [1..99999999]]

repeator :: Int -> [Int]
repeator n = [length (filter (\x -> x >= 1 && isPrime x) [fromDigits k | i <- [1..9], k <- [replace_all_occ a i (digsplit n)]])| a <- [fst m | m <- freq $ digsplit n]]


solution = do
  return (filter (\(_,b) ->  elem 8 b) [(k, repeator k)|k<-prime_list])

main = do
  t0 <- getCurrentTime
  r <- solution
  printTimeSince t0
  print r
  printTimeSince t0
