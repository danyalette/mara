module ReachableReduce where 
  
import DFA 

reachableReduceDFA :: (Eq a) => GenericDFA a b -> GenericDFA a b
reachableReduceDFA dfa = DFA {
    states = dfs (transitions dfa) (initialState dfa) []
    , alphabet = alphabet dfa
    , initialState = initialState dfa 
    , transitions = filter (\(q, s, q') -> (elem q reachableStates) && (elem q' reachableStates)) (transitions dfa)
    , finalStates = finalStates dfa
    , caption = "Reachable States of " ++ (caption dfa)
}
  where reachableStates = dfs (transitions dfa) (initialState dfa) []

dfs :: (Eq a) => [(a, b, a)] -> a -> [a] -> [a]
dfs transitions current visited = 
    foldl (\visited next -> if elem next visited then visited else dfs transitions next visited)
           (visited ++ [current]) (adj current)
           where adj current = map (\(q, s, q') -> q') 
                               (filter (\(q, s, q') -> q == current) transitions)
  
