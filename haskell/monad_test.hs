import Control.Applicative
import Data.Char
-- import Data.Char (chr)


data Expr a = Val a
            | Var String
            | Add [Expr a]
            | Mul [Expr a] deriving Eq

-- Define the Parser type
newtype Parser a = P { parse :: String -> Maybe (a, String) }

-- Parse a single character
-- Týmto by sme mali definovať logiku parseru
item :: Parser Char
item = P $ \input -> case input of
                       ""     -> Nothing
                       (x:xs) -> Just (x, xs)

-- parse item "cde" --> Just ('c', "de")
-- parse1 item "cde" --> Just ('c', "de")

-- Function to run a parser on a string
-- runParser :: Parser a -> String -> Maybe (a, String)
-- runParser (P p) input = p input

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\input ->
    case parse p input of
      Nothing      -> Nothing
      Just (v,out) -> Just (f v, out))

-- fmap f p
-- fmap (== 'c') item

-- parse (fmap (=='c') item) "cde" -->

-- Example usage
-- main :: IO ()
-- main = do
--     let inputString = "Ahoj"
--     case runParser item inputString of
--         Just (parsedChar, remainingString) ->
--             putStrLn $ "Parsed character: " ++ [parsedChar] ++ ", Remaining string: " ++ remainingString
--         Nothing ->
--             putStrLn "Failed to parse character"

instance Applicative Parser where
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                           Nothing      -> Nothing
                           Just (g,out) -> parse (fmap g px) out)

  pure :: a -> Parser a
  pure v = P (\inp -> Just (v,inp))

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                           Nothing      -> Nothing
                           Just (v,out) -> parse (f v) out)

instance Alternative Parser where
  empty :: Parser a
  empty = P (\_ -> Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                         Nothing -> parse q inp
                         Just (v,out) -> Just (v,out))

sat :: (Char -> Bool) -> Parser Char
sat pr = do x <- item
            if pr x then return x
                    else empty
-- Ta dat funkcia príjma nejakú funkciu (Char -> Bool) s názvom pr a vracia nám Parser Char 
-- ono to výsledok z item parseru dá do funkcie pr, tzn nejaký ten Just (Bool, rest of string) 

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

-- Tento program nám umožňuje: parse (char 'a') "agwehj" --> Just ('a',"gwehj")
-- parse (char 'g') "agwehj" --> Nothing
-- Takže ono to nejak upravuje ten parser, ale netuším ako

string :: String -> Parser String
string [] = return []
string (x:xs) = char x
                >> string xs
                >> return (x:xs)

-- Čo robí some operátor v tomto kóde?
-- parse (some (string "abb")) "abbabbabcabcabcaabb_gwehj" --> Just (["abb","abb"],"abcabcabcaabb_gwehj")
-- Aplikuje ten parser rekursívne a potom pridá výsledky do poľa
-- Ak to dáme bez some, tak nám to vyberie iba prvý abb -> parse (string "abb") "abbabbabcabcabcaabb_gwehj" --> Just ("abb","abcabcabcaabb_gwehj")

-- Many vs some
-- many robi to čo some, rozdiel, že many nám môže vrátt prázdny string "" alebo prázdne pole []
-- parse (many (char 'a')) "baaabaabbbbb"
-- Just ("","baaabaabbbbb")
-- parse (some (char 'a')) "baaabaabbbbb"
-- Nothing
-- parse (many (string "ab")) "babababaabbbbb" --> Just ([],"babababaabbbbb")
-- parse (some (string "ab")) "babababaabbbbb" --> Nothing

-- parse (sat isDigit) "abc" --> Nothing
-- parse (sat isDigit) "1abc" --> Just ('1', "abc")

-- parse (some (sat isDigit)) "11abc" --> ("11","abc")
-- parse (sat isDigit) "11abc" --> ('1',"1abc")

helloworld :: IO ()
helloworld = 
  putStrLn "What is your name?" >>
  getLine >>=
  \name -> putStrLn ("Hello " ++ name)

helloworldDoNotation :: IO ()
helloworldDoNotation = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name)

main :: IO ()
main = do
  let result = (+) 3 3
  putChar (chr result)

getSquare :: IO Int
getSquare = putStrLn "Enter number:"
            >> getLine
            >>= \line -> let n = read line
                         in return (n*n)

-- read "3" nefunguje, potrebujeme ešte specifikovať na aký dátový typ ho máme convertovať
-- read "3" :: Int
-- read "3" :: Float
-- read "3" :: Double