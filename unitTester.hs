import Data.List

maybeString :: Maybe String -> String
maybeString mx = case mx of
			Nothing -> ""
			Just x -> x

wantTheLine x lineGet = (a \\ take (x - 1) a) \\ (a \\ take x a) where a = lineGet

wantTheLines x y lineGet = (a \\ take (x - 1) a) \\ (a \\ take y a) where a = lineGet

getTheseLines :: Int -> Int -> [String] -> [String]
getTheseLines a b lineSet = take (b - a + 1) (drop (a - 1) lineSet)
