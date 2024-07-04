module Hw3 where

type Symbol = String
data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq
        --   | Lambda Symbol Expr deriving (Eq, Show)

instance Show Expr where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Lambda x e) = "(\\" ++  x ++ "." ++ show e ++ ")"  

isFree :: Symbol -> Expr -> Bool
isFree y (Var x) = y == x
isFree y (App e1 e2) = isFree y e1 || isFree y e2
isFree y (Lambda x e) = x /= y && isFree y e

doSubstitution :: Symbol -> Expr -> Expr -> Int -> (Expr, Int)
doSubstitution x (Var y) e n | x == y = (e, n)
                             | otherwise = (Var y, n)
doSubstitution x (App e1 e2) e n = (App (fst (doSubstitution x e1 e n)) (fst (doSubstitution x e2 e n)), n)
doSubstitution x (Lambda y e1) e2 n | x == y = (Lambda y e1, n)
                                    | isFree y e2 = doSubstitution x (Lambda ("a" ++ show n) (fst (doSubstitution y e1 (Var ("a" ++ show n)) n))) e2 (n + 1)
                                    | otherwise = (Lambda y (fst (doSubstitution x e1 e2 n)), n)

-- Function to evaluate an expression
eval :: Expr -> Expr
eval e | e == e' = e
       | otherwise = eval e'
    where e' = reduce e

reduce :: Expr -> Expr
reduce (Var x) = Var x                                            -- Var "x"
reduce (App (Lambda x e1) e2) = fst (doSubstitution x e1 e2 0)    -- (App (Lambda x (...)) (...))
reduce (App e1 e2) = App (reduce e1)  (reduce e2)                 -- App (App / Var ...) (App / Var ...)
reduce (Lambda x e) = Lambda x (reduce e)                         -- Lambda "x" (...)
