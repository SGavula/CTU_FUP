module Hw4 where

import Control.Applicative
import Data.Char
import Parser
import Hw3

-- alphaNum :: Parser Char
-- alphaNum = sat isAlphaNum
-- parse alphaNum "3" --> Just ('3', "")
-- parse alphaNum "." --> Nothing
-- alphaNum nemá žiadný argument, lebo podľa mňa je ten alphaNum iba akokeby typ toho parseru, a ten parser už berie string ako argument a vracia Maybe
-- ešte čo je tú dôležité je tá sat funkcia, ktorá berie ako argument nejakú funkciu a aplikuje ju na jeden ten element z vstupu
-- hej a my vlastne pošleme isAlphaNum funkciu do tej sat a ona aplikuje is alphanum na nejaký ten vstupný element
-- A tá isAlphaNum funkcia funguje iba tak, že ju aplikujeme na nejaký char: isAlphaNum 'a' --> true | isAlphaNum '.' --> false
-- a teda ten char máme zo vstupu o čo sa už stará tá sat funkcia, preto pri alphaNum funkcii nemám akokeby ten vstupný parameter, že ono to asi dedí z toho parsera takže preto

-- Potom pri:
-- char :: Char -> Parser Char
-- char c = sat (==c)
-- tu máme ten vstupný parameter Char c lebo ho potrebujeme pri tej funkcii isEqual (==)

-- Ešte si musíme vysvetliť, čo znamená tento function signiture: var :: Parser (Expr a):
-- var :: Parser (Expr a) means that var is a value (or function) of type Parser (Expr a).
-- This means var is a parser that, when applied to some input, produces a result of type Expr a.
-- Takže var je vlastne Parser (ako keby to bola inštancia nejakej triedy v objektovom programovaní)

var :: Parser Symbol
var = some alphaNum

-- <space>*
space :: Parser ()
space = many (sat isSpace) *> pure ()

expr :: Parser Expr
expr = varExpr <|> lambdaExpr <|> appExpr

varExpr :: Parser Expr
varExpr = do 
    x <- var
    return $ Var x


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

definition :: Parser (Symbol, Expr)
definition = do
    v <- var
    _ <- sep
    _ <- string ":="
    _ <- sep
    e <- expr
    return (v, e)

program :: Parser ([(Symbol, Expr)], Expr)
program = do
    defs <- many (definition <* sep)
    e <- expr
    _ <- space
    return (defs, e)

-- Substitute a variable with an expression
substitute :: Symbol -> Expr -> Expr -> Expr
substitute varName varValue (Var x) = if varName == x then varValue else (Var x)
substitute varName varValue (App e1 e2) = App (substitute varName varValue e1) (substitute varName varValue e2)
substitute varName varValue (Lambda x e) = Lambda x (substitute varName varValue e)

makeSubstitution :: [(Symbol, Expr)] -> Expr -> Expr
makeSubstitution [] e = e
makeSubstitution defs programExpr = makeSubstitution (tail defs) (substitute (fst (head defs)) (snd (head defs)) programExpr)

-- Read a program and return the main expression with all definitions resolved
readPrg :: String -> Maybe Expr
readPrg input = case parse program input of
        Just ((defs, programExpr), "") -> Just $ makeSubstitution (reverse defs) programExpr
        Just ((defs, programExpr), _) -> Nothing
        _ -> Nothing
 
-- readPrg :: String -> IO ()
-- readPrg input = case parse program input of
--     Just ((defs, mainExpr), "") -> do
--         putStrLn "Definitions:"
--         print defs
--         putStrLn "Main Expression:"
--         print mainExpr