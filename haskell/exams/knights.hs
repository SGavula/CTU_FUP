{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
data Piece = Nil | Knight deriving Show

validBoard :: [[Piece]]
validBoard = [[Knight, Nil, Nil ,Nil],
            [Nil, Nil, Nil, Knight],
            [Knight, Nil, Nil, Nil],
            [Nil, Knight, Nil, Nil]];

invalidBoard :: [[Piece]]
invalidBoard = [[Nil, Knight, Nil, Nil],
                [Nil, Nil, Nil, Knight],
                [Knight, Nil, Nil, Nil],
                [Nil, Nil, Knight, Nil]];

showPiece :: Piece -> Int -> Int -> String
showPiece p x y = show p ++ ", " ++ show x ++ ", " ++ show y

processSquare :: Int -> Int -> Piece -> (Int, Int)
processSquare rowId colId Knight = (rowId, colId)
processSquare rowId colId Nil = (-1, -1)

processBoard :: [[Piece]] -> [(Int, Int)]
processBoard board = filter (/= (-1, -1)) processedBoard where
    processedBoard = concat $ zipWith (\x -> zipWith (processSquare x) [0..]) [0..] board


difference :: (Int, Int) -> (Int, Int) -> (Int, Int)
difference (x1, x2) (y1, y2) = (abs (x1 - y1), abs (x2 - y2))

checkPoints :: (Int, Int) -> Bool
checkPoints (2, 1) = False
checkPoints (1, 2) = False
checkPoints (x, y) = True

processPoints :: (Int, Int) -> [(Int, Int)] -> [Bool]
processPoints point = map (checkPoints . difference point)

processAllPoints :: [(Int, Int)] -> [Bool]
processAllPoints points = concatMap (filter (== False) . (`processPoints` points)) points

--processAllPoints points = filter (== False) $ concatMap (`processPoints` points) points

process :: [[Piece]] -> Int
process board = length $ processAllPoints points where
    points = processBoard board

is_valid :: [[Piece]] -> Bool
is_valid board = res == 0 where
    res = process board