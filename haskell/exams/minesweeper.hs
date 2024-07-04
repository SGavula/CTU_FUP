import Control.Monad (replicateM)
-- for converting ints to chars
import Data.Char (intToDigit)

-- for testing
test_board = ["..."
             ,".**"
             ,"..."]

slice :: Int -> Int -> [a] -> [a] 
slice numOfDropped numOfTaken board = drop numOfDropped $ take numOfTaken board

-- >>= nam aplikuje slice na kazdy prvok v tom poly
neighbour :: Int -> Int -> [String] -> String
neighbour x y board = slice (y - 1) (y + 2) board >>= slice (x - 1) (x + 2)

numOfMines :: String -> Int
numOfMines neighbours = length $ filter (== '*') neighbours

showElements :: Char -> Int -> Int -> String
showElements e x y = "(" ++ [e] ++ ", " ++ show x ++ ", " ++ show y ++ ")"

sweep :: [String] -> [String]
sweep board = map2d pretty board where
    map2d f = zipWith (\y row -> zipWith (\x e -> f e x y) [0..] row) [0..]
    pretty e x y = finalizer (numOfMines (neighbour x y board)) e
    finalizer _ '*' = '*'
    finalizer 0 _ = '.'
    finalizer n _ = intToDigit n

readInput :: IO [String]
readInput = do
    count <- readLn
    lines <- replicateM (count) getLine
    return lines

main = do
    lines <- readInput
    putStrLn "\nSweep Result:"
    let sw = sweep lines
    mapM_ putStrLn sw
