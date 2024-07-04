-- Exercise 1
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
    show (Leaf x) = "<Leaf " ++ show x ++ "/>"
    show (Node left right) = "<Node>" ++ show left ++ show right ++ "</Node>"

{-
instance Show a => Show (Tree a) where
    show tree = "This is super tree " ++ disp tree ++ " super super tree." where
        disp (Leaf x) = "<Leaf " ++ show x ++ "/>"
        disp (Node left right) = "<Node>" ++ disp left ++ disp right ++ "</Node>"
-}

{-
show tree = disp tree where
disp Leaf a = "<Leaf '" ++ a ++ " '/>"
disp Node (Leaf a) "<Node>" ++ disp Lead a ++ "</Node>"
-}

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) (Leaf 'd')

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
    show Nil = ""
    show (Cons x Nil) = show x
    show (Cons x l) = show x ++ "," ++ show l

-- Exercise 2
-- Tento úkol sa musí robiť pomocou pattern matchingu, nemôžeme to matchovať pomocou guards
treeDepth :: Tree a -> Int
treeDepth (Leaf _) = 1
treeDepth (Node left right) = 1 + max (treeDepth left) (treeDepth right)

labelHlp :: Tree a -> Int -> (Tree (a, Int), Int)
labelHlp (Leaf x) n = (Leaf (x, n), n+1)
labelHlp (Node left right) n = 
    let (newLeft, leftN) = labelHlp left n
        (newRight, rightN) = labelHlp right leftN
    in  ((Node newLeft newRight), rightN)   

labelTree :: Tree a -> Tree (a, Int)
labelTree t = fst (labelHlp t 0)

type Monomial a = (a, Int)

data Polynomial a = Null | Pol (Monomial a) (Polynomial a)

-- This is fine
{-
format :: (Show a, Ord a, Num a) => Monomial a -> String
format (c, e) | e == 0 = show c
              | otherwise = show c ++ "x*^" ++ show e
-}

-- This is better
format :: (Show a, Ord a, Num a) => Monomial a -> String
format (c, e) | e == 0 = display c
              | c == 1 =  "x^" ++ show e
              | otherwise = display c ++ "*x^" ++ show e
              where display n | n >= 0 = show n
                              | otherwise = "(" ++ show c ++ ")"

instance (Show a, Num a, Ord a)  => Show (Polynomial a) where 
    show Null = "0"
    show (Pol m Null) = format m
    show (Pol m k) = format m ++ " + " ++ show k

p = Pol (-1, 0) (Pol (-2, 1) (Pol (1, 3) Null))

getDegree :: Polynomial a -> Int
getDegree Null = -1
getDegree (Pol m k) = max (snd m) (getDegree k)