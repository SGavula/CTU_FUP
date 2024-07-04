f x y = x * y

main:: IO ()
main = do
    let x = 10
    let y = 3
    print $ f x y

separate :: [a] -> ([a], [a])
separate [] = ([], [])
separate (x:[]) =  ([x], [])
separate (x:y:xs) = (x:evs, y:ods) where
    (evs, ods) = separate xs

concatenateStrings :: String -> String -> (String, String)
concatenateStrings str1 str2 = (str1 ++ str2, str2 ++ str1)
