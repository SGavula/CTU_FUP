module Hw3 where

type Symbol = String
data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq

instance Show Expr where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Lambda x e) = "(\\" ++  x ++ "." ++ show e ++ ")"  

isFree :: Symbol -> Expr -> Bool
isFree y (Var x) = y == x
isFree y (App e1 e2) = isFree y e1 || isFree y e2
isFree y (Lambda x e) = x /= y && isFree y e

doSubstitution :: Symbol -> Expr -> Expr -> Int -> (Expr, Int)
-- doSubstitution x (App e1 e2) (Var y) n = (App (fst (doSubstitution x e1 (Var y) n)) (fst (doSubstitution x e2 (Var y) n)), n)
doSubstitution x (Var y) e n | x == y = (e, n)
                             | otherwise = (Var y, n)
doSubstitution x (App e1 e2) e n = (App (fst (doSubstitution x e1 e n)) (fst (doSubstitution x e2 e n)), n)
doSubstitution x (Lambda y e1) e2 n | x == y = (Lambda y e1, n)
                                    | isFree y e2 = doSubstitution x (Lambda ("a" ++ show n) (fst (doSubstitution y e1 (Var ("a" ++ show n)) n))) e2 (n + 1)
                                    | otherwise = (Lambda y (fst (doSubstitution x e1 e2 n)), n)

{-
doSubstitution x (Lambda y e) (Var z) n | x == y = (Lambda y e, n)
                                        | y == z = doSubstitution x (Lambda ("a" ++ show n) (fst (doSubstitution y e (Var ("a" ++ show n)) n))) (Var z) (n+1) -- do alpha reduction
                                        | otherwise = (Lambda y (fst (doSubstitution x e (Var z) n)), n)
-}
-- doSubstitution x (App e1 e2) (Var y) n = (App (fst (doSubstitution x e1 (Var y) n)) (fst (doSubstitution x e2 (Var y) n)), n)
-- doSubstitution x e (App e1 e2) n = (App (fst (doSubstitution x e e1 n)) (fst (doSubstitution x e e2 n)), n)

-- Function to evaluate an expression
eval :: Expr -> Expr
eval e | e == e' = e
       | otherwise = eval e'
    where e' = reduce e

reduce :: Expr -> Expr
reduce (Var x) = Var x
reduce (App e1 e2) =
    case eval e1 of
        Lambda x e -> eval (fst (doSubstitution x e e2 0))
        e1' -> App e1' e2
reduce (Lambda x e) = Lambda x (eval e)

-- reduce (App (Lambda x e1) e2) = reduce (fst (doSubstitution x e1 e2 0))

{-
reduce :: Expr -> Expr
reduce (Var x) = Var x                                            -- Var "x"
reduce (App (Lambda x e1) e2) = fst (doSubstitution x e1 e2 0)    -- (App (Lambda x (...)) (...))
reduce (App e1 e2) = App (eval e1)  (eval e2)                     -- App (App / Var ...) (App / Var ...)
reduce (Lambda x e) = Lambda x (eval e)                           -- Lambda "x" (...)
-}

one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))
suc = Lambda "w"
       (Lambda "y"
         (Lambda "x"
           (App (Var "y")
                (App (App (Var "w") (Var "y"))
                     (Var "x")))))

t1 = App (Var "x") (Var "y") --(x y)
t2 = Lambda "x" (Var "x") -- (\x.x)
t3 = App (Lambda "x" (Var "x")) (Var "y") -- y
t4 = App suc one -- (\y.(\x.(y (y x))))
t5 = App (Lambda "x" (Lambda "x" (Var "x"))) (Lambda "x" (Var "x")) -- (\x.x)
t6 = App (Lambda "x" (Lambda "x" (Var "x"))) (Var "z") -- (\x.x)
t7 = App (Lambda "x" (Lambda "x" (Var "x"))) (Lambda "x" (Var "x")) -- (\x.x)
t8 = App (Lambda "x" (Lambda "y" (Var "x"))) (Lambda "x" (Var "x")) -- (\y.(\x.x))
t9 = App (Lambda "x" (Var "x")) (Lambda "x" (Var "x")) -- (\x.x)
t10 = App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "z") -- (\y.(z y))
t11 = App (Lambda "x" (App (Var "y") (Lambda "y" (App (Var "x") (Var "y"))))) (Var "z") -- (y (\y.(z y)))
t12 = App (Lambda "x" (App (Var "x") (Lambda "y" (App (Var "x") (Var "y"))))) (Var "z") -- (z (\y.(z y))) 
t13 = App (Lambda "x" (Lambda "y" (App (Var "x") (Var "y")))) (Var "y") -- (\a0.(y a0))
t14 = App (Lambda "x" (Lambda "y" (Var "y"))) (Var "y") -- (\a0.a0)
t15 = App (Lambda "x"
           (Lambda "y"
             (Lambda "z" (App (App (Var "x") (Var "y")) (Var "z")))))
         (App (Var "y") (Var "z")) -- (\a0.(\a1.(((y z) a0) a1)))
-- t16 = App (Lambda "z" one) (App (Var "s") (Var "z")) -- (\a0.(\z.(a0 z)))

-- (λx.(λy.x))(λy.(y z))
-- (λx.(λy.x))(λx.(y z))

-- (λy.y)((λz.z z)x)((λz.(λa.a)z)(λy.(λz.z)x)) --> ((x x) (\y.x))
t17_1 = App (Lambda "y" (Var "y")) (App (Lambda "z" (App (Var "z") (Var "z"))) (Var "x"))
t17_2 = App (Lambda "z" (App (Lambda "a" (Var "a")) (Var "z"))) (Lambda "y" (App (Lambda "z" (Var "z")) (Var "x"))) 
t17 = App t17_1 t17_2

-- (λz.((λx.x z)(λy.y z)))(w)
t18 = App (Lambda "z" (App (Lambda "x" (App (Var "x") (Var "z"))) (Lambda "y" (App (Var "y") (Var "z"))))) (Var "w")
t19 = App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x")))
t20 = App (Lambda "x" (Var "y")) (App (Lambda "x" (App (Var "x") (Var "x"))) (Lambda "x" (App (Var "x") (Var "x"))))
-- (λz.((λx.x z)(λy.y z)))(w)
-- (λx.(λy.(x y)))(λy.y)


zero = Lambda "s" (Lambda "z" (Var "z"))
-- one = Lambda "s" (Lambda "z" (App (Var "s") (Var "z")))
two = Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (Var "z"))))
three = Lambda "s" (Lambda "z" (App (Var "s") (App (Var "s") (App (Var "s") (Var "z")))))

true = Lambda "s" (Lambda "z" (Var "s"))
false = zero

neg = Lambda "x" (App (App (Var "x") false) true)

yComb = Lambda "y" (App (Lambda "x" (App (Var "y") (App (Var "x") (Var "x"))))
                        (Lambda "x" (App (Var "y") (App (Var "x") (Var "x"))))
                   )

isNull = Lambda "p" (App (App (Var "p") (Lambda "a" (Lambda "b" neg))) true)

len = Lambda "r"
        (Lambda "lst"
           (App (App (App isNull (Var "lst")) zero)
           (App suc (App (Var "r") (App (Var "lst") false)))))

rlen = App yComb len
test = eval $ App rlen false
expected = zero
