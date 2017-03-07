import Lib
import System.Random
import System.IO.Unsafe
import Data.Function (on)
import Data.List (sortBy, group, sort, maximumBy)
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

ksum :: Int -> [[Int]] -> [Int]
ksum k tr = [tr!!aleph!!beth + sum [sum (maketuple n (tr!!(aleph+n-1))!!beth)| n <- [2..k]] |  aleph <- [0..(length tr)-k], beth <- [0..(length (tr!!aleph))-1]]



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
						   	
mod_tuple :: Int -> [[Int]] -> [[(Int, Int, Int)]]
mod_tuple level tr = [[((aleph!!i)!!0, level, i), ((aleph!!i)!!1, level, i+1),(tr!!(level-1)!!i, level-1, i)] | i <- [0..(level-1)]] where aleph = maketuple 2 (tr!!level)

vertex_summer :: [(Int, Int, Int)] -> (Int, [(Int, Int)])
vertex_summer  = \([(a,r1,c1),(b,r2,c2),(c,r3,c3)]) -> (a + b + c, [(r1, c1), (r2, c2), (r3, c3)])

t2Min = minimum [minimum $ map vertex_summer $ mod_tuple k grid | k <- [1..999]]
t2All = [map vertex_summer $ mod_tuple k grid | k <- [1..999]]

t2 :: Int -> Int -> Int
t2 left_i left_j = fst $ ((t2All!!(left_i-1))!!(left_j))

t3 :: Int -> Int -> Int
t3 i j = (t2 (i-1) j) + (t2 i j) + (t2 i (j+1)) - ((grid!!(i-1)!!j) + (grid!!(i-1)!!(j+1)) + (grid!!i!!(j+1)))

t4 :: Int -> Int -> Int
t4 i j = (t3 i j) + (t2 (i-2) j) + (t2 (i-1) (j+1)) + (t2 i (j+2)) - (grid!!(i-2)!!j) - (grid!!(i-1)!!(j+1)) - (grid!!i!!(j+2)) - (grid!!(i-2)!!(j+1)) - (grid!!(i-1)!!(j+2))

t2fa = [[t2 i j| j<-[0..(i-1)]]|i<-[1..999]]

t :: Int -> Int -> Int -> Int
t n i j | n == 2    = t2fa!!(i-1)!!j
		| otherwise = (t (n-1) i j) + sum [grid!!(i-(n-k))!!(j+k-1)|k<-[1..n]]
		

main = do
  [n] <- getArgs
  let test = [test1] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0
  
test1 = do
  x <- rpar (force $ minimum [t 300 i j|i<-[19..400],  j<-[0..(i-299)]])
  y <- rpar (force $ minimum [t 300 i j|i<-[401..600], j<-[0..(i-299)]])
  --z <- rpar (force $ minimum [t 600 i j|i<-[601..999], j<-[0..(i-599)]])
  rseq x
  rseq y
  --rseq z
  return (x,y) 

