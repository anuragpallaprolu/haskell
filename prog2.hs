digsplit :: Integral x => x -> [x]
digsplit 0 = []
digsplit x = (digsplit (div x 10)) ++ [x `mod` 10]

checkPalindrome :: [Integer] -> Bool
checkPalindrome x | x == [] = True
				  | length x == 1 = True
				  | head x == last x = checkPalindrome (drop 1 (take ((length x) - 1) x))
				  | otherwise = False
				  
a = [100,101,..999]
b = [100,101,..999]

prod_list = [x*y | x <- a, y <- b]

sol = maximum (filter (\x -> checkPalindrome (digsplit x) == True) prod_list)