module Hw3 where

import Data.List

type Symbol = String

data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Lambda x e) = "(\\" ++ x ++ "." ++ show e ++ ")"

-- Function to find all free variables in an expression
freeVars :: Expr -> [Symbol]
freeVars (Var x) = [x]
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Lambda x e) = freeVars e \\ [x]

-- Function to substitute a variable for an expression in another expression
subst :: Symbol -> Expr -> Expr -> Expr
subst x e (Var y) | x == y = e
                  | otherwise = Var y
subst x e (App e1 e2) = App (subst x e e1) (subst x e e2)
subst x e (Lambda y e') | x == y = Lambda y e'
                        | y `notElem` freeVars e = Lambda y (subst x e e')
                        | otherwise = let z = freshVar e in
                                      Lambda z (subst x e (subst y (Var z) e'))

-- Function to generate fresh variables
freshVar :: Expr -> Symbol
freshVar e = head $ filter (`notElem` freeVars e) vars
    where vars = [ "a" ++ show n | n <- [0..] ]

-- Function to evaluate an expression
eval :: Expr -> Expr
eval e | e == e' = e
       | otherwise = eval e'
    where e' = reduce e

-- Function to perform one reduction step
reduce :: Expr -> Expr
reduce (App (Lambda x e1) e2) = subst x e2 e1
reduce (App e1 e2) = if reducible e1 then App (reduce e1) e2 else App e1 (reduce e2)
reduce (Lambda x e) = Lambda x (reduce e)
reduce e = e

-- Function to check if an expression is reducible
reducible :: Expr -> Bool
reducible (App (Lambda _ _) _) = True
reducible (App e1 e2) = reducible e1 || reducible e2
reducible (Lambda _ e) = reducible e
reducible _ = False

ex = App (Lambda "x"
           (Lambda "y"
             (Lambda "z" (App (App (Var "x") (Var "y")) (Var "z")))))
         (App (Var "y") (Var "z"))

one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))
suc = Lambda "w"
       (Lambda "y"
         (Lambda "x"
           (App (Var "y")
                (App (App (Var "w") (Var "y"))
                     (Var "x")))))