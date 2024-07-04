-- module Task4 (minSpanningTree, Graph (..), Edge (..)) where
import Data.List -- for sortOn

data Edge a b = Edge { u :: a,
                       v :: a,
                       weight :: b } deriving (Eq,Show)

data Graph a b = Graph { nodes :: [a],
                         edges :: [Edge a b] } deriving Show

gr :: Graph Char Int
gr = Graph{ nodes = ['A'..'F'],
            edges = [Edge 'A' 'B' 1,
                     Edge 'D' 'E' 4,
                     Edge 'E' 'F' 7,
                     Edge 'A' 'D' 5,
                     Edge 'B' 'E' 2,
                     Edge 'C' 'F' 5,
                     Edge 'D' 'B' 6,
                     Edge 'E' 'C' 4,
                     Edge 'A' 'E' 3] }

reverseEdge :: (Eq a, Ord b) => Edge a b -> Edge a b
reverseEdge (Edge u v weight) = Edge v u weight

extendGraph :: (Eq a, Ord b) => Graph a b -> Graph a b
extendGraph (Graph nodes edges) = let reverseEdges = map reverseEdge edges in
    Graph nodes (reverseEdges ++ edges)

findEdge :: (Eq a, Ord b) => Graph a b -> [a] -> [a] -> Edge a b
findEdge (Graph nodes edges) covered uncovered = head $ sortOn weight $ filter (\(Edge u v _) -> u `elem` covered && v `elem` uncovered) edges

makeMST :: (Eq a, Ord b) => Graph a b -> [a] -> [a] -> [Edge a b] -> [Edge a b]
makeMST _ _ [] edgesMST = edgesMST
makeMST g covered uncovered edgesMST = let (Edge u v weight) = findEdge g covered uncovered
                                           newCovered = covered ++ [v]
                                           newUncovered = filter (/= v) uncovered
                                        in makeMST g newCovered newUncovered (Edge u v weight : edgesMST)

minSpanningTree :: (Eq a, Ord b) => Graph a b -> [Edge a b]
minSpanningTree (Graph nodes edges) = let extendedG = extendGraph (Graph nodes edges)
                                          covered = [head nodes]
                                          uncovered = tail nodes
                                      in makeMST extendedG covered uncovered []