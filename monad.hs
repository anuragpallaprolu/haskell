gets :: Int -> IO String
gets 0 = return []
gets n = getChar >>= \x -> gets (n-1) >>= \xs -> return (x:xs)

--do
--x <- p
--q

--is the same as

--p >>= \x -> q

--As a recursive statement
--do x <- p
--   s
--and this is made into
--p >>= \x -> do s
