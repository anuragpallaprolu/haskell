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
	

isUlam :: [Int] -> Int -> Bool
isUlam ulam n | sum aleph > 1  = False
			  | otherwise      = True where aleph = [1|a<-[x | x <- ulam, x <= div n 2], a /= n-a, elem (n-a) ulam == True]

createUlam :: [Int] -> Int -> Int -> [Int]
createUlam soFar iter test | iter == 0 = soFar
					       | otherwise = createUlam (soFar ++ [test | isUlam soFar test == True]) (iter - 1) (test + 1)