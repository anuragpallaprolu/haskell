import Data.Function (on)
import Data.List (sortBy)
import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

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
		 
aleph = 1000000
a = primes aleph
m = length a

maximum' :: (Ord a, Ord b) => [(a,b)] -> (a,b)
maximum' l = swap $ maximum $ map swap l
             where swap (x, y) = (y, x) 

sum_improved :: Int -> Int -> [Int] -> Int
sum_improved i j a = sum (take (j-i+1) (drop (i-1) a))


subseq a = do
  return (maximum' (filter (\(a,_) -> mod a aleph == a && isPrime a == True) [(sum_improved i j a, j-i)|i <- [1..(length a)], j <- [(i)..(length a)]]))

printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
  
main = do
  t0 <- getCurrentTime
  r <- subseq a
  printTimeSince t0
  print r
  printTimeSince t0
