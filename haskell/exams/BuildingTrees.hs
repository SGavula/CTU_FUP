-- module BuildingTrees (addEdge, buildTree, Tree(..)) where
import Data.List

data Tree a = Leaf { val :: a } 
            | Node { val :: a,
                     kids :: [Tree a] } deriving (Eq,Show) 

type Edge a = (a,a)

addEdge :: (Eq a, Ord a) => Tree a -> Edge a -> Tree a
addEdge (Leaf n0) (n1, c) = if n0 == n1 then Node n0 [Leaf c] else Leaf n0
addEdge (Node n0 kids) (n1, c) = if n0 == n1 then Node n0 (sortOn val ((Leaf c):kids)) else Node n0 (map (\k -> addEdge k (n1, c)) kids)

buildTree :: Ord a => Tree a -> [Edge a] -> Tree a
buildTree tree edges = foldl addEdge tree edges