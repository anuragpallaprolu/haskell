import Control.Lens


main = do
	 contents <- getContents
	 putStr (shortLines contents)
	 

shortLines :: String -> String
shortLines input = unlines (filter (\line -> length line < 10) (lines input))

lineRead :: IO ()
lineRead = do contents <- readFile filename
              case drop 6 $ lines contents of
                []  -> error "File has less than seven lines"
                l:_ -> putStrLn l
  where filename = "in.txt"
  
  
