{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.List

type Symbol = String
data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Lambda x e) = "(\\" ++  x ++ "." ++ show e ++ ")"

doSubstitution :: Symbol -> Expr -> Expr -> Expr  
doSubstitution x (App e1 e2) (Var y) = App (doSubstitution x e1 (Var y)) (doSubstitution x e2 (Var y))
doSubstitution x (Var y) (Var z) | x == y = Var z
                                 | otherwise = Var y
doSubstitution x (Var y) e | x == y = e
                           | otherwise = Var y
doSubstitution x (Lambda y e) (Var z) | x == y = Lambda y e
                                      | y == z = let e' = alphaConvert e y (freshVar e)
                                                     in doSubstitution x (Lambda (freshVar e) e') (Var z)
                                      | otherwise = Lambda y (doSubstitution x e (Var z))
doSubstitution x (Lambda y e1) e2 | x == y = Lambda y e1
                                  | otherwise = Lambda y (doSubstitution x e1 e2)
doSubstitution x (App e1 e2) e = App (doSubstitution x e1 e) (doSubstitution x e2 e)
doSubstitution x e (App e1 e2) = App (doSubstitution x e e1) (doSubstitution x e e2)

-- Function to evaluate an expression
eval :: Expr -> Expr
eval e | e == e' = e
       | otherwise = eval e'
    where e' = reduce e

reduce :: Expr -> Expr
reduce (Var x) = Var x                                    -- Var "x"
reduce (App (Lambda x e1) e2) = doSubstitution x e1 e2    -- (App (Lambda x (...)) (...))
reduce (App e1 e2) = App (eval e1)  (eval e2)             -- App (App / Var ...) (App / Var ...)
reduce (Lambda x e) = Lambda x (eval e)    



-- Function to generate fresh variables
freshVar :: Expr -> Symbol
freshVar e = head $ filter (`notElem` freeVars e) vars
    where vars = [ "a" ++ show n | n <- [0..] ]

-- Function to find all free variables in an expression
freeVars :: Expr -> [Symbol]
freeVars (Var x) = [x]
freeVars (App e1 e2) = nub $ freeVars e1 ++ freeVars e2
freeVars (Lambda x e) = filter (/= x) (freeVars e)

one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))
suc = Lambda "w"
       (Lambda "y"
         (Lambda "x"
           (App (Var "y")
                (App (App (Var "w") (Var "y"))
                     (Var "x")))))

t13 = App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "y") -- (\a0.(y a0))
t14 = App (Lambda "x" (Lambda "y" (Var "y"))) (Var "y") -- (\a0.a0)
t15 = App (Lambda "x"
           (Lambda "y"
             (Lambda "z" (App (App (Var "x") (Var "y")) (Var "z")))))
         (App (Var "y") (Var "z"))
t16 = App (Lambda "z" (Lambda "s" (Lambda "z" (App (Var "s") (Var "z"))))) (App (Var "s") (Var "z")) -- (\a0.(\z.(a0 z)))