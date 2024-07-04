separate :: [Int] -> ([Int], [Int])
separate [] = ([], [])
separate [x] = ([x], [])
separate (x:y:xs) = let (even, odd) = separate (xs) in (x:even, y:odd)

separate2 :: [Int] -> ([Int], [Int])
separate2[] = ([], [])
separate2[x] = ([x], [])
separate2 (x:y:xs) = (x:even, y:odd) where (even, odd) = separate2 (xs)

isEven :: Int -> Bool
-- isEven (-1) = False
-- isEven 0 = True
isEven n
        | n == (-1) = False
        | n == 0 = True
        | n < 0 = isEven (n + 2)
        | otherwise = isEven (n - 2)

-- Vidíme, že ten pattern matching sa správa rovnako ako guards

stringAppend ::  Int -> String -> String -> String
stringAppend 1 str acc = str ++ acc
stringAppend n str acc = stringAppend (n - 1) str str ++ acc


numToStr :: Int -> Int -> String
numToStr n radix = let
        x = div n radix
        y = mod n radix
        chars = ['0'..'9'] ++ ['A'..'F']
        in if n < radix then [chars !! y] else numToStr x radix ++ [chars !! y]

-- Takto môžem vypisovať môj kód
{-
main :: IO ()
main = do
    putStrLn $ numToStr 52 10
    putStrLn $ numToStr 5 2
    putStrLn $ numToStr 255 16
-}

split :: Int -> [Int] -> [[Int]]
split n xs =
        if length xs <= n
        then [take n xs]
        else (take n xs) : (split n (drop n xs))

average_n :: Int -> [Int] -> [Float]
average_n n xs = let
        listOfSplittedNums = split n xs
        in [fromIntegral (sum x) / fromIntegral (length x) | x <- listOfSplittedNums]

copy :: Int -> String -> String
copy n str = if n == 1 then str else str ++ copy (n - 1) str

copyTail :: Int -> String -> String
copyTail n str = iter n "" where
        iter k acc | k == 1 = str ++ acc
                   | otherwise = iter (k - 1) (str ++ acc)


luhnDouble :: Int -> Int
luhnDouble n = let
        n2 = 2 * n
        in if n2 >= 10 then n2 - 9 else n2

luhn :: [Int] -> Bool
luhn nums = let reversedNums = reverse nums
                numsList = separate reversedNums
                sumEven = sum (fst numsList)
                listOdds = [luhnDouble x | x <- snd numsList]
                sumOdds = sum listOdds
                sumTogether = sumOdds + sumEven
                in mod sumTogether 10 == 0

-- data List a = Nil | Cons a (List a) deriving Show
data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
  show lst = "[" ++ disp lst ++ "]" where
    disp Nil = ""
    disp (Cons x Nil) = show x
    disp (Cons x l) = show x ++ "," ++ disp l

myList :: List Int
myList = Cons 1 (Cons 2 (Cons 3 Nil))

data Expr a = Val a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)

instance Show a => Show (Expr a) where
    show (Val x) = show x
    show (Add e1 e2) = "(" ++ show e1
                           ++ " + "
                           ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1
                           ++ " * "
                           ++ show e2 ++ ")"

eval :: (Num a) => Expr a -> a
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
