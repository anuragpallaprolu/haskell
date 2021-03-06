fib :: (Integral a) => a -> a
fib 0 = 0
fib n = fibhelper n 0 1

fibhelper :: (Integral a) => a -> a -> a -> a
fibhelper 1 x y = y
fibhelper n x y = fibhelper (n-1) y (x+y)