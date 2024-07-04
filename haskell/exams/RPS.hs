-- module RPS (rps) where
import Data.List

players = ["alice", "bob", "charlie"]
strategies = [['r', 'p'], ['r', 'r'], ['s', 'p']]

players1 = ["alice", "bob", "charlie"]
strategies1 = [['r'], ['r'], ['s']]

players2 = ["alice", "bob", "charlie"]
strategies2 = [['r', 'p', 'r'], ['p', 's', 'r'], ['s', 'r', 'p']]

isFinished xs = null xs || any null xs

currentStrategies = map head

futureStrategies = map tail

getLostStrategy :: String -> Char
getLostStrategy "pr" = 'r'
getLostStrategy "ps" = 'p'
getLostStrategy "rs" = 's'
getLostStrategy _ = 'x'

makeMask :: String -> [Bool]
makeMask str = let lostStrategy =  getLostStrategy $ sort $ nub str
               in map (\x -> if (x == lostStrategy) then False else True) str

filterList :: [a] -> [Bool] -> [a]
filterList lst mask = map fst $ filter snd $ zip lst mask

rps :: [String] -> [[Char]] -> [String]
rps players strategies | isFinished strategies = players
rps players strategies =
    let current = currentStrategies strategies
        future = futureStrategies strategies
        mask = makeMask current
    in rps (filterList players mask) (filterList future mask)