import Data.List
import System.Random

permute :: Eq a => [a] -> [[a]]
permute [] = [[]]
permute xs = [x:ys | x <- xs, ys <- permute (delete x xs)]

distance :: [Char] -> [Char] -> Int
distance x y = maybeInt (elemIndex x (permute (sort x))) - maybeInt (elemIndex y (permute (sort x)))
			 

maybeInt :: Maybe Int -> Int
maybeInt mx = case mx of 
	Nothing -> 0 
	Just x -> x
	
giuRandom x = return.(x!!)=<<System.Random.randomRIO(0,length x-1)


nextPerm x = head [y | y <- permute (sort x), distance x y == 1]
