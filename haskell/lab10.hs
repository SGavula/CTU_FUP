{-
interleave :: a -> [a] -> [[a]] 
interleave x [] = [[x]]
interleave x (y:ys) = x:y:ys
-}

-- permutations :: [a] -> [[a]]
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

toUpper :: Char -> Char
toUpper c = case lookup c (zip ['a'..'z'] ['A'..'Z']) of
    Nothing -> c
    Just c' -> c'

{-
toCamelCase :: String -> String
toCamelCase str = toUpper (head str) : tail str
-}

-- my implemenation of toCamelCase

toCamelCase :: String -> String
toCamelCase inp = concat [toUpper (head str) : tail str | str <- strings]
                    where strings = words inp

toCamelCaseF :: Functor f => f String -> f String
toCamelCaseF = fmap toCamelCase
