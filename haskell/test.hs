import Control.Applicative

-- Define a function to add a greeting to a string
addGreeting :: String -> String -> String
addGreeting greeting name = greeting ++ ", " ++ name ++ "!"

-- Define two Maybe String computations
maybeGreeting :: Maybe String
maybeGreeting = Just "Hello"

maybeName :: Maybe String
maybeName = Just "Alice"

-- Use the *> operator to sequence computations and discard the result of the second one
resultWithGreeting :: Maybe String
resultWithGreeting = addGreeting <$> maybeGreeting *> maybeName

-- Use the <* operator to sequence computations and discard the result of the first one
resultWithName :: Maybe String
resultWithName = addGreeting <$> maybeGreeting <* maybeName

-- Print the results
main :: IO ()
main = do
    putStrLn "Result with greeting and name:"
    print resultWithGreeting  -- Just "Hello, Alice!"
    
    putStrLn "\nResult with name only:"
    print resultWithName      -- Just "Hello, !"