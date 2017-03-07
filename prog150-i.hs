import Lib

maketuple :: Int -> [Int] -> [[Int]]
maketuple n (x:xs) | length xs < n  = [x:xs]
				   | otherwise       = take n (x:xs) : maketuple n xs 

test_triangle = [[1],[2,3],[4,5,6],[7,8,9,10]] :: [[Int]]

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



