import Control.Parallel.Strategies

f a = a * a

x = 1
y = 2
			 

runEval (rpar (f x) >>= (\a -> rpar (f y) >>= (\b -> return(a,b))))
   