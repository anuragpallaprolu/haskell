import System.Random
import System.IO.Unsafe
import Data.Function (on)
import Data.List (sortBy)
import Data.Function (on)
import Data.List (sortBy)
import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

-- Miller-Rabin wrapped up as an (almost deterministic) pure function
isPrime :: Int -> Bool
isPrime n = unsafePerformIO (isMillerRabinPrime 100 n)
 
 
isMillerRabinPrime :: Int -> Int -> IO Bool
isMillerRabinPrime k n
   | even n    = return (n==2)
   | n < 100   = return (n `elem` primesTo100)
   | otherwise = do ws <- witnesses k n
                    return $ and [test n (pred n) evens (head odds) a | a <- ws]
  where
    (evens,odds) = span even (iterate (`div` 2) (pred n))
 
test :: Integral nat => nat -> nat -> [nat] -> nat -> nat -> Bool
test n n_1 evens d a = x `elem` [1,n_1] || n_1 `elem` powers 
  where
    x = powerMod n a d
    powers = map (powerMod n a) evens
 
witnesses :: (Num a, Ord a, Random a) => Int -> a -> IO [a]
witnesses k n 
  | n < 9080191         = return [31,73]
  | n < 4759123141      = return [2,7,61]
  | n < 3474749660383   = return [2,3,5,7,11,13]
  | n < 341550071728321 = return [2,3,5,7,11,13,17]
  | otherwise           = do g <- newStdGen
                             return $ take k (randomRs (2,n-1) g)
 
primesTo100 :: [Int]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
 
-- powerMod m x n = x^n `mod` m
powerMod :: Integral nat => nat -> nat -> nat -> nat
powerMod m x n  = f (n - 1) x x `rem` m 
  where
  f d a y = if d==0 then y else g d a y 
  g i b y | even i    = g (i `quot` 2) (b*b `rem` m) y
          | otherwise = f (i-1) b (b*y `rem` m)
		  
primes :: Int -> [Int]
primes x | x == 1 = []
		 | x < 1 = error "WTAF"
		 | otherwise = [y | y <- [1..x], isPrime y == True]
		 
aleph = 100000
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