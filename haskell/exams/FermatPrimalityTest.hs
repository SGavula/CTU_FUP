import Control.Monad.State

data LCG = LCG  Int Int Int Int deriving Show

generate :: State LCG Int
generate = do (LCG a x c m) <- get
              let x' = (a*x + c) `mod` m
              put (LCG a x' c m)
              return x'

generate_range :: Int -> Int -> State LCG Int
generate_range lower upper = do b <- generate
                                let b' = (b `mod` (upper - lower)) + lower
                                return b'

iterativeMod :: Int -> Int -> Int -> Int
iterativeMod a p 0 = 1 `mod` p
iterativeMod a p k = a * (iterativeMod a p (k - 1)) `mod` p 

fermatCheck :: Int -> Int -> Int -> State LCG Bool
fermatCheck num numOfRep 1 = primality num (numOfRep - 1) 
fermatCheck _ _ _ = return False

primality :: Int -> Int -> State LCG Bool
primality num 0 = return True
primality num numOfRep = do a <- generate_range 1 num
                            fermatCheck num numOfRep (iterativeMod a num (num - 1))