import Lib

isValid :: [[Int]] -> Bool
isValid grid = ((sum [grid!!k!!k|k<-[0..3]]) == (sum [grid!!k!!(3-k)|k<-[0..3]])) && ((sum [grid!!k!!(3-k)|k<-[0..3]]) == sum (grid!!0)) && (sum (grid!!0)== sum (grid!!1)) && (sum (grid!!1) =	= sum (grid!!2)) && (sum (grid!!2) == sum (grid!!3))

