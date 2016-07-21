import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO
import Control.Monad

-- main = do
--       let lineSet = []
--       loadLines "case.dat" lineSet
--       return(lineSet)

loadLines path list = do
                      inh <- openFile path ReadMode
                      mainloop inh list
                      hClose inh

mainloop inh list = do
                    ineof <- hIsEOF inh
                    if ineof then return () else do
                                                 inpLine<- hGetLine inh
                                                 let list = inpLine:list
                                                 mainloop inh list

getLines = liftM lines . readFile

getTheseLines a b = do
                    list <- getLines "case.dat"
                    return(take (b - a + 1) (drop (a - 1) list))

s = getTheseLines 1 5

numberArray a b linenumber = do
                             array <- getTheseLines a b
                             return(map read $ words $ array !! linenumber :: [Int])

monadicNumberArray a b linenumber = (\array -> map read $ words $ array !! linenumber :: [Int]) <$> getTheseLines a b 
-- main = do
--       t <- numberArray 1 5 3
--       return (t !! 2)

--Remember ApplicativeDo is just syntatic sugar for monadic bindings
--do
--x <- a
--y <- b
--return(f x y)
--Can be written as (\x y -> f x y) <$> a <*> b
--THE TYPE OF A AND B IS AN IO MONAD!


main = (\t -> t !! 2) <$> numberArray 1 5 3
