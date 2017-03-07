module Lib where 


import System.Random
import System.IO.Unsafe
import Data.Function (on)
import Data.List (sortBy, sort, group)
import Data.List (union)
import Control.Parallel
import Control.Parallel.Strategies
import Control.Exception
import Data.Time.Clock
import Text.Printf
import System.Environment

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

choose :: [b] -> Int -> [[b]]
_      `choose` 0       = [[]]
[]     `choose` _       =  []
(x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

--choose is a  much much faster sublistofsize.



digsplit :: Integral x => x -> [x]
digsplit 0 = []
digsplit x = (digsplit (div x 10)) ++ [x `mod` 10]

divides :: Int -> Int -> Bool
divides d n = rem n d == 0

exclude :: Int -> [Int] -> [Int]
exclude k x = (take (k-1) x) ++ (drop k x)

freq l = filter (\(_,b) -> b > 1) [(k,count k l)|k<-(rmdups l)]

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0 where addDigit num d = 10*num + d

isPrime n | n < 1     = error "Not a pos int"
		  | n == 1    = False
		  | otherwise = ld n == n

ld :: Int -> Int
ld n = ldf 2 n

ldf :: Int -> Int -> Int
ldf k n | divides k n = k
		| k^2 > n 	  = n
		| otherwise   = ldf (k+1) n

maximum' :: (Ord a, Ord b) => [(a,b)] -> (a,b)
maximum' l = swap $ maximum $ map swap l
             where swap (x, y) = (y, x) 

permute :: [Int] -> [[Int]]
permute x | length x == 2 = [[(head x),(last x)],[(last x),(head x)]] 
		  | otherwise = [(x!!(a-1)):b | a <- [1..(length x)], b <- (permute (exclude a x))]
			 
primes :: Int -> [Int]
primes x | x == 1 = []
		 | x < 1 = error "WTAF"
		 | otherwise = [y | y <- [1..x], isPrime y == True]
			 
printTimeSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)

replace :: Int -> Int -> [Int] -> [Int]
replace i n x = (take (i-1) x) ++ [n] ++ (drop i x) 

replace_l :: [Int] -> Int -> [Int] -> [Int]
replace_l a n x | length a == 1 = replace (head a) n x
			    | otherwise     = replace_l (tail a) n (replace (head a) n x)


replace_all_occ :: Int -> Int -> [Int] -> [Int]
replace_all_occ i j x | count i x == 0 = x
					  | otherwise      = [if tx == i then j else tx|tx <- x] 

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

sublistofsize 0 _        = [[]]
sublistofsize _ []       = []
sublistofsize n (x : xs) = sublistsThatStartWithX ++ sublistsThatDontStartWithX
  where sublistsThatStartWithX = map (x:) $ sublistofsize (n-1) xs
        sublistsThatDontStartWithX = sublistofsize n xs
		
sum_improved :: Int -> Int -> [Int] -> Int
sum_improved i j a = sum (take (j-i+1) (drop (i-1) a))

convertToBase :: Integer -> Integer -> [Integer]
--Given an integer and a base, return the integer in that base
convertToBase x b | x == 0    = []
				  | otherwise = (convertToBase (div x b) b) ++ [(mod x b)]







				





		  







