import Data.List


data Point = TwoD Float Float | ThreeD Float Float Float deriving Show

direction :: Point -> Point -> Point -> Int

direction (TwoD a1 a2) (TwoD b1 b2) (TwoD c1 c2) | atan((b2 - a2)/(b1 - a1)) - atan((c2 - b2)/(c1 - b1)) > 0 = 1
												 | atan((b2 - a2)/(b1 - a1)) - atan((c2 - b2)/(c1 - b1)) < 0 = -1
												 | otherwise = 0 
												 
												 
guardedTail x | null x = [] | otherwise = tail x										 
pointHead :: [Point] -> Point
pointHead [] = TwoD 0.0 0.0
pointHead (x:xs) = x

hullDirection :: [Point] -> [Int]
hullDirection [] = []
hullDirection (x:xs) = (direction x (pointHead xs) (pointHead ys)):hullDirection xs where ys = (guardedTail xs)

 
