--Round E APAC 2017
--Problem B
--Beautiful Numbers

import Lib
import Data.Time.Clock
import Control.Parallel
import Control.Parallel.Strategies
import System.Environment
import Control.Exception
import Control.DeepSeq

--Takes a number and gives the base in which it is beautiful
--The base in which the number is represented by all 1s is the beautiful base.
--If there are multiple bases with all 1s then choose the one with the maximum 1s.

checkones :: [Integer] -> Bool
checkones x = (x == [1 | y <- [1..length(x)]])

beautiful n end_here iter bsoFar nib | iter == end_here = bsoFar
								     | otherwise = beautiful n end_here (iter + 1) (if ((checkones $ convertToBase n iter) == True) && (length $ convertToBase n iter) > nib then iter else bsoFar) (if ((checkones $ convertToBase n iter) == True) && (length $ convertToBase n iter) > nib then (length $ convertToBase n iter) else nib) 

main = do
  [n] <- getArgs
  let test = [test1] !! (read n - 1)
  t0 <- getCurrentTime
  r <- evaluate (runEval test)
  printTimeSince t0
  print r
  printTimeSince t0
  
test1 = do
  x <- rpar (force $ beautiful 123456789 123456 2 2 0)
  y <- rpar (force $ beautiful 123456789 1234567 123457 2 0)
  z <- rpar (force $ beautiful 123456789 12345678 1234568 2 0)
  w <- rpar (force $ beautiful 123456789 99999999 12345679 2 0)
  rseq x
  rseq y
  rseq z
  rseq w
  return (x,y,z,w) 
				  
