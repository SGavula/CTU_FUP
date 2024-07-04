module Hw4 where

import Control.Applicative
import Data.Char
import Parser
import Hw3

space :: Parser ()
space = many (sat isSpace) *> pure ()

var :: Parser [Char]
var = some alphaNum
-- parse var "abcd." --> Just ("abcd", ".")

varExpr :: Parser Expr
varExpr = do 
    x <- var
    return $ Var x
-- parse varExpr "abcd1." --> Just (Var "abcd1",".")

appExpr :: Parser Expr
appExpr = do
    _ <- char '('
    e1 <- expr
    _ <- sep
    e2 <- expr
    _ <- char ')'
    return $ App e1 e2

lambdaExpr :: Parser Expr
lambdaExpr = do
    _ <- char '('
    _ <- char '\\'
    v <- var
    _ <- char '.'
    e <- expr
    _ <- char ')'
    return $ Lambda v e 

expr :: Parser Expr
expr = varExpr <|> appExpr <|> lambdaExpr
-- parse expr "(\\s.(\\z.z))" --> Just (Lambda "s" (Lambda "z" (Var "z")),"")

definition :: Parser (String, Expr)
definition = do
    v <- var
    _ <- sep
    _ <- string ":="
    _ <- sep
    e <- expr
    return (v, e)
-- parse definition "I := (\\x.x)\n (I I)" --> Just (("I",(\x.x)),"\n (I I)")

program :: Parser ([(String, Expr)], Expr)
program = do
    xs <- many (definition <* sep)
    e <- expr
    _ <- space
    return $ (xs, e)

varsSubstitute :: Symbol -> Expr -> Expr -> Expr
varsSubstitute varName varValue (Var x) = if varName == x then varValue else (Var x)
varsSubstitute varName varValue (App e1 e2) = App (varsSubstitute varName varValue e1) (varsSubstitute varName varValue e2)
varsSubstitute varName varValue (Lambda x e) = Lambda x (varsSubstitute varName varValue e)

makeSubstitution :: [(Symbol, Expr)] -> Expr -> Expr
makeSubstitution [] e = e
makeSubstitution defs programExpr = makeSubstitution (tail defs) (varsSubstitute (fst (head defs)) (snd (head defs)) programExpr)

readPrg :: String -> Maybe Expr
readPrg s = case parse program s of
            Just ((xs, e),"") -> Just $ makeSubstitution (reverse xs) e 
            Just ((xs, e), _) -> Nothing
            _ -> Nothing
