import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

data FTree a = FNil | FNode (Map a (FTree a)) deriving (Eq, Show)

files =  ["src/tree.hs"
         ,"src/complex.hs"
         ,"scripts/ex1/test.ss"
         ,"scripts/ex1/eval.ss"
         ,"scripts/emptydir"
         ,"scripts/ex2/test.ss"
         ,"tests/test_tree.hs"]


split :: String -> [String]
split path = case (break (== '/') path) of
    (s, "") -> [s]
    (s1, s2) -> [s1] ++ split (drop 1 s2)

-- concat $ map split files --> ["src","tree.hs","src","complex.hs","scripts","ex1","test.ss","scripts","ex1","eval.ss","scripts","emptydir","scripts","ex2","test.ss","tests","test_tree.hs"]

insert :: [String] -> FTree String -> FTree String
insert [] tree = tree
insert (s:r) FNil = FNode (Map.insert s (insert r FNil) Map.empty)
insert (s:r) (FNode t) = let oldTree = fromMaybe FNil (Map.lookup s t)
                        in FNode (Map.insert s (insert r oldTree) t)

parse :: [String] -> FTree String
parse = foldl (\tree file -> insert (split file) tree) FNil
-- parse files = foldl (\tree file -> insert (split file) tree) FNil files


helperExists :: [String] -> FTree String -> Bool
helperExists [] tree = True
helperExists _ FNil = False
helperExists (s1:s2) (FNode tree) = 
    case Map.lookup s1 tree of
        Nothing -> False
        Just t -> helperExists s2 t

exists :: String -> FTree String -> Bool
exists string tree = helperExists (split string) tree