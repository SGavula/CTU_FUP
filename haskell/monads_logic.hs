import Language.Haskell.TH (javaScript)
-- Rozdiel medzi functorom a monadom môžeme sledovať na tomto príklade:
half x = if even x
    then Just2 (x `div` 2)
    else Nothing2

-- Functor: fmap half (Just 4) --> Just (Just 2)
-- Monad: (Just 4) >>= half --> (Just 2)

-- Definícia monadu
-- class Monad m where
--     (>>=) :: m a -> (a -> m b) -> m b

data Maybe2 a = Just2 a | Nothing2 deriving Show

instance Functor Maybe2 where
    fmap f (Just2 a) = Just2 (f a)
    fmap f Nothing2 = Nothing2

instance Applicative Maybe2 where
    pure = Just2
    Just2 f <*> j = fmap f j
    Nothing2 <*> j = Nothing2

instance Monad Maybe2 where
    Nothing2 >>= f = Nothing2
    -- Just2 a >>= f = Just (f a) Pozor my to nemusíme wrappovať opäť do Just
    Just2 a >>= f = f a

-- Teraz keď spustím tento kód: (Just2 4) >>= half --> (Just2 2) 

-- Príklad so stromčekom
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

-- Functor
instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- Applicative
instance Applicative Tree where
    pure = Leaf
    Leaf f <*> t = fmap f t
    Branch left right <*> t = Branch (left <*> t) (right <*> t)

-- Definujeme funkciu, ktorú chceme použiť s monadom
g x | x == 4 = Leaf 400
    | otherwise = Leaf (x * 2)

-- My chceme niečo takéto: x >>= g, kde x = Branch (Leaf 5) (Branch (Leaf 6) (Leaf 7))

instance Monad Tree where
    Leaf a >>= f = f a
    Branch left right >>= f = Branch (left >>= f) (right >>= f)

-- Conclusion: Ak tomu správne chápem, tak monad iba vyberie tú hodnotu z toho wrapperu a pushne to do funkcie a to je všetko