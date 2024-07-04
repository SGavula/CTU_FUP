-- Aplicatives je type class
-- Môžeme nad tým uvažovať ako nad interfacom, tzn definovať logiku všetkých metód

-- Definícia applicative
-- class Functor f => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b

-- Aby sme si ukázali príklad applicative, tak si musíme ukázať, že v haskelli sú okrem wrapped values aj wrapped funkcie
-- Napríklad (Just (+3)) je wrapped funkcia

-- Opäť defalutne nefunguje ak chceme pracovať s wrapped funkciou a wrapped hodnotou
-- Toto: (Just (+3)) (Just 9) --> error
-- Applicatives slúžia na to, aby sme s tým dokázali pracovať

-- <*> toto je symbol applicative, rovnako ako totot: <$> je symbol functoru
--  použijeme applicative na tento kód: (Just (+3)) (Just 9) --> error
-- (Just (+3)) <*> (Just 9) --> (Just 12)
-- <*> je infix operátor

-- Príklad s datovým typom Maybe2
data Maybe2 a = Just2 a | Nothing2 deriving Show

-- Functor definition
instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2 = Nothing2

-- Applicative definition
instance Applicative Maybe2 where
    pure = Just2 -- pure = Just2 a
    -- Just2 f <*> (Just2 j) = Just (f j)
    Just2 f <*> j = fmap f j -- to su akokeby 2 zápisi a ten druhý je akokeby automaticky tam už využívame Functor
    Nothing2 <*> j = Nothing2

-- Teraz už nám fungujú všetky tieto funkcie:
-- (Just2 (+3)) <*> (Just2 9) --> (Just2 12)
-- (Just2 (+3)) <*> Nothing2 --> Nothing2
-- Nothing2 <*> (Just2 (+3)) --> Nothing2
-- (Just2 (+3)) *> (Just2 9) --> (Just2 9)
-- (Just2 (+3)) <* (Just2 9) --> error
-- fmap (+) (Just 2) <*> (Just 3) --> (Just 5) je to ekvivalentné s týmto zápisom (+) <$> (Just 2) <*> (Just 3)
-- (Just 5) <* (Just 2) --> (Just 5)
-- (Just 5) *> (Just 2) --> (Just 2)


-- Príklad so stromčekom
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

-- Functor
instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- Pomocou functoru môžem dať: fmap (*3) Branch (Leaf 5) (Branch (Leaf 6) (Leaf 7))
-- Ale nemôžem dať toto: fmap (Leaf (*3)) Branch (Leaf 5) (Branch (Leaf 6) (Leaf 7))
-- Ale ak si definujeme applicative, tak môžeme urobiť toto: (Leaf (+3)) <*> Branch (Leaf 5) (Branch (Leaf 6) (Leaf 7))

-- Applicative
instance Applicative Tree where
    pure = Leaf
    Leaf f <*> t = fmap f t
    Branch left right <*> t = Branch (left <*> t) (right <*> t)

-- (Leaf (+3)) <*> (Leaf 5) (Branch (Leaf 6) (Leaf 7)) --> (Leaf 8) (Branch (Leaf 9) (Leaf 10))
