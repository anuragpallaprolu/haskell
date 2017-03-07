--Hacking the n^2 bound on Ulam sequences
--A. Pallaprolu

import Lib
import Prelude
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


diffTuple :: [Int] -> Int -> [Int]
diffTuple ulam n | (sum [1|a<-[x | x <- ulam, x <= div n 2], a /= n-a, elem (n-a) ulam == True]) == 1 = ulam ++ [n]
				 | otherwise                                                                          = ulam

createUlam :: [Int] -> Int -> Int -> [Int]
createUlam soFar test n | test == n = soFar
					    | otherwise = createUlam (diffTuple soFar test) (test + 1) (n)
						
ulamF n = foldl' diffTuple [1,2] [3..n]