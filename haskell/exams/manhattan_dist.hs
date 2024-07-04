-- module Manhattan (grid) where
import Data.Char
import Data.List
import Data.Binary.Get (label)

points :: [(Char, Int, Int)]
points = [
       ('A', 1, 1),
       ('B', 1, 6),
       ('C', 8, 3),
       ('D', 3, 4),
       ('E', 5, 5),
       ('F', 8, 9)]

helper :: Char -> Int -> Int -> String
helper a b c = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++  ")"

first :: (Char, Int, Int) -> Char
first (a, _, _) = a
second :: (Char, Int, Int) -> Int
second (_, a, _) = a
third :: (Char, Int, Int) -> Int
third (_, _, a) = a

sortDist :: [(a, Int)] -> [(a, Int)]
sortDist = sortBy (\(_,a) (_,b) -> compare a b)

manDist :: (Int, Int) -> (Int, Int) -> Int
manDist (x1, x2) (y1, y2) = abs (x1 - y1) + abs (x2 - y2)

getMaxCol :: [(Char, Int, Int)] -> Int
getMaxCol lst = maximum $ map second lst

getMaxRow :: [(Char, Int, Int)] -> Int
getMaxRow lst = maximum $ map third lst

calDist :: [(Char, Int, Int)] -> (Int, Int) -> [(Char, Int)]
calDist points (x, y) = map (\(a, b, c) -> (a, manDist (x, y) (b, c))) points

-- calDist :: (Int, Int) -> [(Char, Int, Int)] -> [(Char, Int)]
-- calDist (x, y) = map (\(a, b, c) -> (a, manDist (x, y) (b, c)))
-- ide to aj bez tych points

getLetter :: [(Char, Int)] -> Char
getLetter ((l, 0):_) = l
getLetter ((l1, n1):(_, n2):_) = if n1 == n2 then '.' else toLower l1  

test :: [(Char, Int, Int)] -> [[Char]]
test points = [[test2 (x, y) | x <- [0..mx]] | y <- [0..my]] where
    mx = getMaxCol points
    my = getMaxRow points
    test2 = getLetter . sortDist . calDist points


-- grid :: [(Char,Int,Int)] -> [[Char]]
-- -- grid = Implement me!

