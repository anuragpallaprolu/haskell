import Data.List

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Eq,Ord,Show)	

treeFromList :: (Ord a) => [a] -> BinTree a
treeFromList [] = Empty
treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs)) (treeFromList (filter (>x) xs))

indent :: [String] -> [String]
indent = map("-->"++)

prettyTree :: Show a => BinTree a -> [String]
prettyTree Empty = []
prettyTree (Node here left right) = indent (prettyTree right) ++ [show here] ++ indent (prettyTree left)

printTree :: Show a => BinTree a -> String
printTree = unlines.prettyTree
