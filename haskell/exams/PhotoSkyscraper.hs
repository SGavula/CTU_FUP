module PhotoSkyscraper (bestView) where
import Data.Text.Internal.Lazy.Fusion (countChar)
import Data.List

city :: [[Int]]
city = [[3, 3, 3],
        [1, 2, 3],
        [1, 2, 3]]

calcCount :: [Int] -> Int -> Int -> Int -> Int
calcCount [] count max_val a = if a > max_val then (count + 1) else count
calcCount lst count max_val a = if a > max_val then calcCount (tail lst) (count + 1) a (head lst) else calcCount (tail lst) count max_val (head lst)

calcForWest :: [[Int]] -> Int
calcForWest city = sum lst where
    lst = map (\x -> calcCount (tail x) 1 (head x) (head x)) city

calcForEast :: [[Int]] -> Int
calcForEast city = sum lst where
    lst = map ((\x -> calcCount (tail x) 1 (head x) (head x)) . reverse) city
    -- map (\x -> calcCount (tail x) 1 (head x) (head x)) (map reverse city)

calcForNorth :: [[Int]] -> Int
calcForNorth city = sum lst where
    lst = map (\x -> calcCount (tail x) 1 (head x) (head x)) (transpose city)

calcForSouth :: [[Int]] -> Int
calcForSouth city = sum lst where
    lst = map ((\x -> calcCount (tail x) 1 (head x) (head x)) . reverse) (transpose city)

bestView :: [[Int]] -> (Char, Int)
bestView city = let lst = [('N', calcForNorth city), ('S', calcForSouth city), ('W', calcForWest city), ('E', calcForEast city)]
                    sortedLst = sortBy (\(_,a) (_,b) -> compare a b) lst
                in last sortedLst

{-
bestView :: [[Int]] -> (Char, Int)
bestView city = let lst = ('N', calcForNorth city) : ('S', calcForSouth city) : ('W', calcForWest city) : ('E', calcForEast city) : []
                    sortedLst = sortBy (\(_,a) (_,b) -> compare a b) lst
                in last sortedLst
-}
