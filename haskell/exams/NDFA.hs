data Transition a b = Tr a b a deriving Show
data Automaton a b = NFA [Transition a b] a [a] 

nfa::Automaton Int Char
nfa = NFA [Tr 1 'a' 2,
           Tr 2 'b' 2,
           Tr 1 'a' 3,
           Tr 3 'b' 4,
           Tr 4 'a' 3,
           Tr 2 'a' 4]
           1
           [2,3]

next :: (Eq a, Eq b) => a -> b -> [Transition a b] -> [Transition a b]
next state symbol trans = filter (\(Tr initState symbolBetween toState) -> (initState == state) && (symbolBetween == symbol)) trans 

nextAllStates :: (Eq a, Eq b) => [a] -> b -> [Transition a b] -> [a]
nextAllStates states symbol trans = let resTrans = concat $ map (\x -> next x symbol trans) states
                                    in map (\(Tr _ _ toState) -> toState) resTrans

getFinalStates :: (Eq a, Eq b) => [b] -> Automaton a b -> [a]
getFinalStates symbols (NFA trans initState finalStates) = foldl (\states symbol -> nextAllStates states symbol trans) [initState] symbols

checkFinalStates :: Eq a => [a] -> [a] -> Bool
checkFinalStates states finalStates = let processedStates = concat $ map (\x -> filter (== x) finalStates) states
                                      in ((length processedStates) > 0)

accepts :: (Eq a, Eq b) => Automaton a b -> [b] -> Bool
accepts (NFA trans initState finalStates) symbols = let resStates = getFinalStates symbols (NFA trans initState finalStates)
                                                    in checkFinalStates resStates finalStates

generateWords :: Eq b => [b] -> Int -> [[b]]
generateWords symbols 1 = map (\x -> [x]) symbols
generateWords symbols n = let symbolsList = map (\x -> [x]) symbols 
                          in concat $ map (\x -> map (\y -> y ++ x) symbolsList) (generateWords symbols (n-1))  

lwords :: (Eq a, Eq b) => [b] -> Automaton a b -> Int -> [[b]]
lwords symbols automaton 0 = []
lwords symbols automaton n = let words = generateWords symbols n
                             in filter (\x -> (accepts automaton x)) words