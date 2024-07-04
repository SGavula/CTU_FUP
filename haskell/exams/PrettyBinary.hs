type Img = [String]

zero :: Img
zero = [".##.",
        "#..#",
        "#..#",
        ".##."]

one :: Img
one =  ["...#",
        "..##",
        "...#",
        "...#"]

zeroWithDot :: Img
zeroWithDot = map (\x -> x ++ ".") zero

oneWithDot :: Img
oneWithDot = map (\x -> x ++ ".") one

numToBin :: Int -> Bool -> [[String]]
numToBin 0 b = if b == True then [zero] else [oneWithDot]
numToBin 1 b = if b == True then [one] else [oneWithDot]
numToBin n b = if (n `mod` 2) == 1 then 
    (if b == True then numToBin (n `div` 2) False ++ [one] else numToBin (n `div` 2) False ++ [oneWithDot])
    else 
    (if b == True then numToBin (n `div` 2) False ++ [zero] else numToBin (n `div` 2) False ++ [zeroWithDot])

processString :: [[String]] -> [[String]]
processString lst = if null (head lst) then [] else [map head lst] ++ processString (map tail lst)

main :: IO ()
main = do
    putStrLn "Enter an integer: "
    input <- getLine
    let n = read input :: Int
    let strBin = numToBin n True
    let processStrBin = processString strBin
    let strBinWithEnter = map (\x -> x ++ ["\n"]) processStrBin
    let finalStr = concat $ map concat strBinWithEnter
    putStrLn finalStr