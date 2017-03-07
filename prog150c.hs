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

vertexsums :: [[Int]] -> Int -> Int -> Int -> Int
vertexsums tr size i j | size == 2 = tr!!i!!j + tr!!(i+1)!!j + tr!!(i+1)!!(j+1)
					   | otherwise = sum [tr!!(i+size-1)!!m | m <- [j..(j+size-1)]] + vertexsums tr (size-1) i j

vmin :: [[Int]] -> Int -> Int -> Int -> Int -> Int
vmin tr current_min iter i j | iter == 1000-i        = current_min
							 | aleph < current_min = vmin tr aleph (iter+1) i j
							 | otherwise           = vmin tr current_min (iter+1) i j where aleph = vertexsums tr iter i j
							 
				
main = do
  [n] <- getArgs
  let test = [test1] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0
  
test1 = do
  x <- rpar (force $ minimum [vmin grid 0 2 500 j| j <- [0..40]])
  y <- rpar (force $ minimum [vmin grid 0 2 500 j| j <- [40..80]])
  z <- rpar (force $ minimum [vmin grid 0 2 500 j| j <- [80..120]])
  rseq x
  rseq y
  rseq z
  return (x, y, z) 