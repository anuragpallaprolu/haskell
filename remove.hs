import Data.Char

remove :: String -> String

remove [] = []
remove (a:st)
	| notPunct a = a : remove st
	| otherwise = remove st

notPunct ch = isAlpha ch || isDigit ch