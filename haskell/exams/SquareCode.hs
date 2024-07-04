-- module SquareCode ( encode ) where
import Data.Char
import Data.List

groupList :: String -> Int -> Int -> String -> [String]
groupList "" _ _ acc = (reverse acc) : []
groupList string n 0 acc = (reverse acc) : groupList string n n []
groupList string n k acc = groupList (tail string) n (k - 1) ((head string) : acc)  

addSpaceEnd :: String -> Int -> String
addSpaceEnd string n = let len = (length string) in 
    string ++ (replicate (n-len) ' ')

addSpaceAll :: [String] -> Int -> [String]
addSpaceAll lst n = map (\x -> addSpaceEnd x n) lst

processString :: String -> String
processString s = filter isAlpha $ map toLower s

groupAndTransposeString :: String -> Int -> [String]
groupAndTransposeString s n = transpose $ groupList s n n []

encode :: String -> String
encode s = let processedString = processString s
               n = ceiling $ sqrt $ fromIntegral $ length processedString
               lst = groupAndTransposeString processedString n 
               lstWithSpace = addSpaceAll lst n
               res = concat lstWithSpace
               in take (length res - 1) res