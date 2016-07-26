--Conway with Lists
--Much better than all that brainfuck with Arrays


import Data.List
import Control.Lens

initGrid :: Int -> [Int]
initGrid n = [0 | x <- [1..(n^2)+1]] -- +1 because the cuntbag chunksOf doesn't run with the usual nonsense.

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr (test . splitAt n) where
  test (_, []) = Nothing
  test x       = Just x

printGrid a n = mapM_ print $ chunksOf n a

updateGrid :: Int -> Int -> Int -> Int -> [Int] -> [Int]
updateGrid i j n v a = a & element t .~ v where t = n*(i-1) + j - 1 

arrayTolist :: Int -> Int -> Int -> Int
arrayTolist x y size = size * (x - 1) + y - 1 

listToarray :: Int -> Int -> (Int, Int)
listToarray x size = (t,x-size*(t-1)) where t = (div x size) + 1


listNeighbors :: [Int] -> Int-> Int -> Int -> [Int]
listNeighbors grid x y size | x == 1 && y == 1 = [grid !! x | x <- [(arrayTolist 1 2 size),(arrayTolist 2 1 size),(arrayTolist 2 2 size)]]
                            | x == 1 && y /= 1 && y /= size = [grid !! x | x <- [(arrayTolist 1 (y-1) size),(arrayTolist 1 (y+1) size),(arrayTolist 2 (y-1) size),(arrayTolist 2 (y) size),(arrayTolist 2 (y+1) size)]] 
                            | x /= 1 && x /= size && y == 1 = [grid !! x | x <- [(arrayTolist (x-1) 1 size),(arrayTolist (x+1) 1 size),(arrayTolist (x-1) 2 size),(arrayTolist x 2 size),(arrayTolist (x+1) 2 size)]]
                            | x == size && y == size = [grid !! x | x <- [(arrayTolist (size-1) (size-1) size),(arrayTolist (size-1) size size),(arrayTolist size (size-1) size)]] 
                            | x == size && y /= 1 && y /= size = [grid !! x | x <- [(arrayTolist size (y-1) size),(arrayTolist size (y+1) size),(arrayTolist (size-1) (y-1) size),(arrayTolist (size-1) y size),(arrayTolist (size-1) (y+1) size)]]
                            | x == size && y == 1 = [grid !! x | x <- [(arrayTolist (size-1) 2 size),(arrayTolist (size-1) 1 size),(arrayTolist size 2 size)]] 
                            | x /= size && x /= 1 && y == size = [grid !! x | x <- [(arrayTolist (x-1) size size),(arrayTolist (x+1) size size),(arrayTolist (x-1) (size-1) size),(arrayTolist x (size-1) size),(arrayTolist (x+1) (size-1) size)]]
                            | x == 1 && y == size = [grid !! x | x <- [(arrayTolist 2 size size),(arrayTolist 2 (size-1) size),(arrayTolist 1 (size-1) size)]]
                            | otherwise = [grid !! x | x <- [(arrayTolist (x-1) (y-1) size),(arrayTolist (x-1) (y) size),(arrayTolist (x-1) (y+1) size),(arrayTolist (x) (y-1) size),(arrayTolist (x) (y+1) size),(arrayTolist (x+1) (y-1) size),(arrayTolist (x+1) (y) size),(arrayTolist (x+1) (y+1) size)]]

--This is all pain.


sumNeighbors grid x y  size = sum $ listNeighbors grid x y size

cellUpdate :: [Int] -> Int -> Int -> Int -> Int
cellUpdate grid x y size | grid !! (arrayTolist x y size) == 1 && sumNeighbors grid x y size < 2 = 0
                         | grid !! (arrayTolist x y size) == 1 && (sumNeighbors grid x y size == 2 || sumNeighbors grid x y size == 3) = 1
                         | grid !! (arrayTolist x y size) == 1 && sumNeighbors grid x y size > 3 = 0
                         | grid !! (arrayTolist x y size) == 0 && sumNeighbors grid x y size == 3 = 1
                         | otherwise = grid !! (arrayTolist x y size)
newGrid grid size = [cellUpdate grid x y size | x <- [1..size], y <- [1..size]]

life :: [Int] -> Int-> Int -> IO()
life grid size 0 = printGrid grid size
life grid size k = life (newGrid grid size) size (k-1)

life2 :: [Int] -> Int -> Int -> IO()
life2 grid size 0 = printGrid grid size
life2 grid size k = printGrid grid size >> life2 (newGrid grid size) size (k-1)