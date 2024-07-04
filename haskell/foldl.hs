-- Ukážme si ako sa používa foldl funkcia: foldl func initial_value array_cez_ktore_loopojume
-- Define a function to add two numbers
add :: Int -> Int -> Int
add x y = x + y

-- Use foldl to sum a list
sumList :: [Int] -> Int
sumList xs = foldl add 0 xs

concatenate :: String -> String -> String
concatenate s1 s2 = s1 ++ " " ++ s2

concatenateList :: [String] -> String
concatenateList xs = foldl concatenate "" xs

-- Example usage
main :: IO ()
-- main = print $ sumList [1, 2, 3, 4, 5]  -- Output: 15
main = print $ concatenateList ["Hello", "World", "How", "Are", "You"] -- Output: Hello World How Are You