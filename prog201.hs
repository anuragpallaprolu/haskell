import Lib
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set


uniq :: [Int] -> [Int]
uniq x = [x!!i|i <- [0..((length x)-1)], elem (x!!i) (exclude (i+1) x) == False]

set = [x*x | x <- [1..100]]

remember :: Ord a => a -> State (Set a) ()
remember a = modify (Set.insert a)


