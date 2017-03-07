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

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
					
takeCommon :: [Int] -> [Int] -> [Int]
takeCommon x y = (x \\ intersect x y) ++ (y \\ intersect x y)

takeCommon2 :: [Int] -> [Int] -> [Int]
takeCommon2 (x:xs) y | y == []          = (x:xs)
                     | xs == []         = x:y
					 | elem x y == True = xs ++ (removeItem x y)
					 | otherwise        = takeCommon2 xs y
					 
takeCommonf x y = rmdups $ takeCommon2 x y

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . sg

sg :: Ord a => [a] -> [[a]]
sg = group . sort


unique :: Ord a => [a] -> [a]
unique = concat . filterByLength (==1)
					
sumLastPair :: [Int] -> [Int]
sumLastPair a = [b + l | b <- a, b /= l] where l = last a

ulamSeq :: Int -> Int -> [Int] -> [Int] -> ([Int],[Int])
ulamSeq n iter ulamSoFar acc | iter == 0 = (ulamSoFar, acc)
							 | otherwise = ((fst a) ++ [head . sort $ filter (\x -> elem x (snd a) == False) (sumLastPair (fst a))], sort $ (snd a) ++ ((sumLastPair (fst a)))) where a = ulamSeq n (iter - 1) ulamSoFar acc
			
all2Sums a = rmdups [x + y | x <- a, y <- a, x /= y]