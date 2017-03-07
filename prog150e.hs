import Lib
import System.Random
import System.IO.Unsafe
import Data.Function (on)
import Data.List (sortBy, group, sort)
import Control.Parallel
import Control.Parallel.Strategies
import Data.Time.Clock
import Text.Printf
import System.Environment
import Control.Exception
import Control.DeepSeq



maketuple :: Int -> [Int] -> [[Int]]
maketuple n (x:xs) | length xs < n  = [x:xs]
				   | otherwise       = take n (x:xs) : maketuple n xs 

test_triangle = [[1],[2,3],[4,5,6],[7,8,9,10]] :: [[Int]]

construct_tr :: Int -> Int -> [Int]
construct_tr iter t | iter == 0 = []
					| otherwise = [(mod (615949*t + 797807) (2^20)) - (2^19)] ++ construct_tr (iter-1) (mod (615949*t + 797807) (2^20))
					
finalizer :: Int -> [Int] -> [[Int]]
finalizer n_row c = [take n (drop (div (n*(n-1)) 2) c) |n <- [1..n_row]] 

grid = finalizer 1000 $ construct_tr 500500 0

twomin :: Int -> Int -> [[Int]] -> Int
twomin current_min iter tr | iter == 999         = current_min
						   | aleph < current_min = twomin aleph (iter+1) tr 
						   | otherwise           = twomin current_min (iter+1) tr where aleph = minimum [tr!!iter!!k + tr!!(iter+1)!!k + tr!!(iter+1)!!(k+1) | k <- [0..iter]]
						   
mmin :: Int -> Int -> Int -> [[Int]] -> Int
mmin m current_min iter tr | iter == 1001-m      = current_min
						   | aleph < current_min = mmin m aleph (iter+1) tr
						   | otherwise           = mmin m current_min (iter+1) tr where aleph = minimum [(sum [tr!!(iter+i)!!(k+j)|i <- [0..(m-1)], j<- [0..i]]) | k <- [0..iter]]

						   
main = do
  [n] <- getArgs
  let test = [test1] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0
  
test1 = do
  x <- rpar (force $ minimum [mmin k 0 0 grid|k <- [21..30]])
  y <- rpar (force $ minimum [mmin w 0 0 grid|w <- [31..40]])
  rseq x
  rseq y
  return (x, y)  


