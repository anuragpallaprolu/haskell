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
maketuple :: Int -> [Int] -> [[Int]]
maketuple n (x:xs) | length xs < n  = [x:xs]
				   | otherwise       = take n (x:xs) : maketuple n xs 

test_triangle = [[1],[2,3],[4,5,6],[7,8,9,10]] :: [[Int]]

twosum :: [[Int]] -> [Int]
twosum tr = [tr!!aleph!!beth + sum (maketuple 2 (tr!!(aleph+1))!!beth) | aleph <- [0..(length tr)-2], beth <- [0..(length (tr!!aleph))-1]]


ksum :: Int -> [[Int]] -> [Int]
ksum k tr = [tr!!aleph!!beth + sum [sum (maketuple n (tr!!(aleph+n-1))!!beth)| n <- [2..k]] |  aleph <- [0..(length tr)-k], beth <- [0..(length (tr!!aleph))-1]]


evaluate n_row tr = do
  return (minimum [minimum (ksum k tr)| k <- [2..n_row]])

construct_tr :: Int -> Int -> [Int]
construct_tr iter t | iter == 0 = []
					| otherwise = [(mod (615949*t + 797807) (2^20)) - (2^19)] ++ construct_tr (iter-1) (mod (615949*t + 797807) (2^20))
					
finalizer :: Int -> [Int] -> [[Int]]
finalizer n_row c = [take n (drop (div (n*(n-1)) 2) c) |n <- [1..n_row]] 

grid = finalizer 1000 $ construct_tr 500500 0

main = do
  t0 <- getCurrentTime
  r <- evaluate 1000 grid
  printTimeSince t0
  print r
  printTimeSince t0



