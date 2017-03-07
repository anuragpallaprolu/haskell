--Hacking the n^2 bound on Ulam sequences
--Algorithm taken from the paper : "The Use Of Bit And Byte Manipulation In Computing Summation Series" by M. C. Wunderlich, 1971.
--Programmed in Haskell 8.0.2
--A. Pallaprolu
--STMicroelectronics, Noida, India.

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

v = [1,1] ++ [0 | x <- [1..]] :: [Int]


vsel :: [Int] -> Int -> Int -> [Int]
vsel v a b = take (b-a+1) (drop (a-1) v)

sumL :: [Int] -> [Int] -> [Int]
sumL [] a = a
sumL a [] = a
sumL (x:xs) (y:ys) | y == 1    = (x+y):(sumL xs ys)
				   | otherwise = x:(sumL xs ys)

updatev :: [Int] -> Int -> [Int]
--update the bit vector with this function
--updatev vsofar k = vnewsofar
--v(k+1, 2k-1) = v(k+1, 2k-1) + v(1, k-1)
updatev vec k =  (vsel vec 1 (k)) ++ (sumL (vsel vec (k+1) (2*k - 1)) (vsel vec 1 (k-1))) ++ (vsel vec (2*k) (length vec))

updatek :: Int -> [Int] -> Int -> (Int, Bool)
updatek k vec n | aleph == [] = (k, True)
				| k     > 500 = (k, True)
				| otherwise   = (minimum aleph, False) where aleph = [j | j <- [(k+1)..(length vec)], vec!!(j-1) == n]

				
ulamBit :: [Int] -> Bool -> Int -> [Int]
ulamBit usoFar isDone k | isDone == True     = [j | j <- [1..(length usoFar)], usoFar !! (j-1) == 1]
						| otherwise          = ulamBit upv (snd $ updatek k upv 1) (fst $ updatek k upv 1) where upv = (updatev usoFar k)
						

 

