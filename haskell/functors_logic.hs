-- Functor is a TYPECLASS
-- behave as interface

-- Definition of functor
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- (+3) is a function in haskell, it adds 3 to its argument
-- funguje to vďaka curryingu Int -> (Int -> Int) => (+3) => 3 -> (Int -> Int)
-- https://stackoverflow.com/questions/11217874/in-haskell-is-a-function-2-is-a-function-2-3-is-5-what-exact

-- Unwrapped value: 9, 4, etc

-- Wrapped value: (Just 3)

-- ja nemôžem urobiť toto: (+3) (Just 9) --> error
-- Tu nám pomôže FUNCTOR (fmap) fmap je akokeby ten funktor, je to také mätúce, lebo ono sa to správa ako map
-- Takže toto fmap (+3) (Just 9) mi už bude fungovať a vráti mi to: (Just 12), teda fmap (+3) (Just 9) --> (Just 12)
-- Ten maybe datatype má už ten funkctor defalutne nastavený

-- My si ale poďme urobiť svoj vlastný maybe datatype --> maybe2

data Maybe2 a = Just2 a | Nothing2 deriving Show
-- Tu už nám ale nebude fungovať: fmap (+3) (Just2 9) --> error
-- Preto si mi musíme vytvoriť svoj vlastný functor, ktorý bude pracovať s datatypom maybe2

-- !! POZOR nepoužijeme class ale instance a namiesto f dávame Maybe2
-- class Functor f where
-- máme 2 prípady, datatype Maybe2 môže mať 2 hodnoty: Just 2 a a potom Nothing2
instance Functor Maybe2 where
    fmap func (Just2 a) = Just2 (func a)
    fmap func Nothing2 = Nothing2

-- teraz už nám funguje: fmap (+3) (Just2 9) --> (Just2 12)
-- aj toto nám funguje: fmap (+3) Nothing2 --> Nothing2

-- fmap --> prefix zápis functoru: fmap (+3) (Just2 9), pričom toto nefunguje: (+3) fmap (Just2 9)
-- <$> --> infix zápis functoru: (+3) <$> (Just2 9), pričom toto nefunguje: <$> (+3) (Just2 9)

-- Príkalad na datovej štruktúre Tree respektíve dátovom type Tree
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show
-- Branch (Leaf 5) (Branch (Leaf 6) (Leaf 7))
-- My chceme teraz na každé a-čko tzn. na každé čísielko použiť nejakú funkciu f, v našom prípade (+3)
-- Urobíme to pomocou functora

instance Functor Tree where
    fmap func (Leaf a) = Leaf (func a)
    fmap func (Branch left right) = Branch (fmap func left) (fmap func right) -- Branch (func <$> left) (func <$> right)

-- !! Pozor nefunguje to s funkciou (-3), lebo je asi nejak divne definovaná
-- Preto ak chceme odpočítať od (Just n) číslo 3, tak namiesto (-3) to musíme definovať ako (\x -> x -3)

-- Conclusion:
-- Functor (fmap) je spôsob / funkcia, ktorá nám umožňuje aplikovať funkciu na wrapped values tzn. zabalené hodnoty