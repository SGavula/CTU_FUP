-- module BalancedTree ( Tree (..), buildHeap) where
data Tree a = Leaf | Node a (Tree a) (Tree a)

tostr :: (Show a) => Tree a -> Int -> String
tostr Leaf d = ""
tostr (Node x l r) d = tostr l (d+1) ++ concat (replicate d "---") ++ show x 
                       ++ "\n" ++  (tostr r (d+1))
instance (Show a) => Show (Tree a) where
    show tree = tostr tree 0

minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node v left right) = min (minDepth left) (minDepth right) + 1 

heapify :: (Eq a, Ord a) => Tree a -> Tree a
heapify Leaf = Leaf
heapify (Node noveV Leaf Leaf) = Node noveV Leaf Leaf
heapify (Node nodeV left Leaf) = let (Node leftV llt lrt) = heapify left in 
    Node (max nodeV leftV) (Node (min nodeV leftV) llt lrt) Leaf
heapify (Node nodeV left right) = let (Node leftV llt lrt) = heapify left 
                                      (Node rightV rlt rrt) = heapify right
                                      heapifyLeftFirst = if leftV > nodeV then Node leftV (Node nodeV llt lrt) right else Node nodeV left right
                                      (Node newNodeV newLeft newRight) = heapifyLeftFirst
    in if rightV > newNodeV then Node rightV newLeft (Node newNodeV rlt rrt) else Node newNodeV newLeft newRight

buildTree :: (Eq a, Ord a) => Tree a -> a -> Tree a
buildTree Leaf v = Node v Leaf Leaf
buildTree (Node nodeV left right) v = if minDepth left > minDepth right then heapify (Node nodeV left (buildTree right v)) else heapify(Node nodeV (buildTree left v) right)    

buildHeap :: (Eq a, Ord a) => [a] -> Tree a
buildHeap [] = Leaf
buildHeap lst = buildTree (buildHeap (tail lst)) (head lst)
-- buildHeap lst = foldl buildTree Leaf (reverse lst)