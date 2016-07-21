import System.IO
import Control.Monad

deriv f x  = (f (x + dx) - f x)/dx where dx = 0.001

newton eps f guess = if (abs ((guess - (f guess / deriv f guess)) - guess) < eps) then guess - (f guess / deriv f guess) else newton eps f (guess - (f guess / deriv f guess))


jane m guess eps cost_array = newton eps f guess - 1 where f x = poly cost_array x m

dot :: [Double] -> [Double] -> Double
dot [] [] = 0.0
dot (x:xs) (y:ys) = x*y + dot xs ys

pol_Construct :: Double -> Int -> [Double]
pol_Construct x 0 = [1]
pol_Construct x m = (x^m):pol_Construct x (m-1)

poly cost_array x m = dot cost_array (pol_Construct x m)
