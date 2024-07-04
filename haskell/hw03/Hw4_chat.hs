module Hw4 where

import Control.Applicative
import Data.Char
import Parser
import Hw3

-- Parser for variables (alphanumeric strings)
var :: Parser Symbol
var = some alphaNum

-- Parser for expressions
expr :: Parser Expr
expr = varExpr <|> lambdaExpr <|> appExpr

varExpr :: Parser Expr
varExpr = Var <$> var

lambdaExpr :: Parser Expr
lambdaExpr = do
    _ <- char '('
    _ <- char '\\'
    v <- var
    _ <- char '.'
    e <- expr
    _ <- char ')'
    return $ Lambda v e

appExpr :: Parser Expr
appExpr = do
    _ <- char '('
    e1 <- expr
    _ <- sep
    e2 <- expr
    _ <- char ')'
    return $ App e1 e2

-- Parser for definitions
definition :: Parser (Symbol, Expr)
definition = do
    v <- var
    _ <- sep
    _ <- string ":="
    _ <- sep
    e <- expr
    return (v, e)

-- Parser for the entire program
program :: Parser ([(Symbol, Expr)], Expr)
program = do
    defs <- many (definition <* sep)
    e <- expr
    _ <- many (sat isSpace) -- Allow trailing spaces
    return (defs, e)

-- Substitute variable with expression in another expression
substitute :: Symbol -> Expr -> Expr -> Expr
substitute var val (Var v) = if v == var then val else Var v
substitute var val (Lambda v e) = if v == var then Lambda v e else Lambda v (substitute var val e)
substitute var val (App e1 e2) = App (substitute var val e1) (substitute var val e2)

-- Resolve all definitions in the main expression
resolveDefs :: [(Symbol, Expr)] -> Expr -> Expr
resolveDefs defs mainExpr = foldl (\acc (v, e) -> substitute v (resolveDefs defs e) acc) mainExpr defs

-- Read and parse a lambda calculus program
readPrg :: String -> Maybe Expr
readPrg input = case parse program input of
    Nothing -> Nothing
    Just ((defs, mainExpr), _) -> Just $ resolveDefs defs mainExpr
