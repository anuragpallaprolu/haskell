--Hacking the n^2 bound on Ulam sequences
--A. Pallaprolu

import Lib
import Data.List
import System.Random
import System.IO.Unsafe
import Data.Function (on)
import Control.Parallel
import Control.Parallel.Strategies
import Data.Time.Clock
import Text.Printf
import System.Environment
import Control.Exception
import Control.DeepSeq


diffTuple :: [Int] -> Int -> [(Int, Int)]
diffTuple ulam n = [(a, n-a)|a<-[x | x <- ulam, x <= div n 2], a /= n-a, elem (n-a) ulam == True]

createUlam :: [Int] -> Int -> Int -> [Int]
createUlam soFar iter test | iter == 0 = soFar
					       | otherwise = createUlam (soFar ++ [test | length (diffTuple soFar test) == 1]) (iter - 1) (test + 1)

main = do
  [n] <- getArgs
  let test = [test1] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0
  
test1 = do
  x <- rpar (force $ createUlam [1,2] 1000 3)
  y <- rpar (force $ createUlam [1,2] 100000 3)
  --z <- rpar (force $ minimum [t 600 i j|i<-[601..999], j<-[0..(i-599)]])
  rseq x
  rseq y
  --rseq z
  return (x,y) 