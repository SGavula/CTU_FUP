{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Eta reduce" #-}
data Expr a = Val a
            | Var String
            | Add [Expr a]
            | Mul [Expr a] deriving Eq

newtype Parser a = P { parse :: String -> Maybe (Expr a, String)}

item :: Parser Char
item = P (\input -> case input of
                      ""     -> Nothing
                      (x:xs) -> Just (Val x, xs))

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\input ->
    case parse p input of
      Nothing      -> Nothing
      Just (v,out) -> Just (f v, out))

parse :: String -> Maybe (Expr (a -> b), String)

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
sat pr = item >>= \x -> if pr x then return x
                                else empty

alphaNum :: Parser Char
alpahNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = return []
string (x:xs) = char x
                >> string xs
                >> return (x:xs)