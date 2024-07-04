-- module Ancestor (findPath, commonAncestor, Tree(..)) where

data Tree a = Leaf a
            | Node a (Tree a) (Tree a) deriving (Eq,Show)

tree = Node 1 (Node 2 (Leaf 5) (Leaf 6)) (Node 3 (Leaf 4) (Leaf 7))

tree2 = Node 1 (Node 2 (Leaf 3)
                       (Node 4 (Leaf 5)
                               (Leaf 6)))
               (Node 7 (Leaf 8)
                       (Leaf 9))

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

findPathHelper :: Eq a => a -> [a] -> [a]
findPathHelper _ [] = []
findPathHelper x lst = x:lst

findPath :: Eq a => a -> Tree a -> [a]
findPath x (Leaf v) = if (x == v) then [x] else []
findPath x (Node v left right) = if (x == v) then [x] else (let leftLst = findPath x left
                                                                rightLst = findPath x right
                                                                both = (leftLst ++ rightLst)
                                                            in findPathHelper v both)

commonAncestor :: Eq a => a -> a -> Tree a -> Maybe a
commonAncestor x y tree = let xLst = findPath x tree
                              yLst = findPath y tree
                              ancestors = commonPrefix xLst yLst
                          in if (length ancestors) == 0 then Nothing else Just (last ancestors)  
