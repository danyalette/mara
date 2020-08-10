module Interleaving (makeInterleavedProgram) where 
  
import DFA 

-- Convert a list of transitions :: [(state, symbol, state')] into a 
-- function which takes (state, symbol) and returns [state']
makeTransitionFunction :: (Eq a, Eq b) => [(a,b,a)] -> ((a, b) -> [a]) 
makeTransitionFunction transitions = \(q, s) -> map 
          (\(x, y, z) -> z)
          (filter (\(x, y, z) -> x == q && y == s) transitions)

-- Convert a symbol "a" to a tuple ("a", i) for a fixed integer i  
-- and convert a state "q" to a list ["q"] (to simplify interleaving 
-- procedure - details in interleave comment)
addThreadIndexToDFA :: DFA -> Int -> GenericDFA [String] (String, Int) 
addThreadIndexToDFA dfa i = DFA {
      states = map (\x -> [x]) (states dfa)
    , alphabet = map (\x -> (x,i)) (alphabet dfa) 
    , initialState = [initialState dfa]
    , transitions = map (\(q, s, q') -> ([q], (s, i), [q'])) (transitions dfa)
    , finalStates = map (\x -> [x]) (finalStates dfa)
    , caption = caption dfa
    }

-- Note that, for simplicity of implementation, interleave requires each state of 
-- an input DFA to be a list. This allows us to form the product state as 
-- e.g. ([q1, q2, q3] , [q4]) = [q1, q2, q3, q4], without dealing with 
-- flattening lists or differing state types. 
-- Todo: It would be more elegant to create a class such a Productable which 
-- the state type must instantiate, and which implements a function that takes the 
-- product of two states. 
interleave :: (Eq a, Eq b) => GenericDFA [a] b -> GenericDFA [a] b -> String -> GenericDFA [a] b
interleave dfa1 dfa2 caption = DFA {
    states = [ x ++ y | x <- (states dfa1), y <- (states dfa2)] 
    , alphabet = (alphabet dfa1) ++ (alphabet dfa2)
    , initialState = initialState dfa1 ++ initialState dfa2
    , transitions = interleaveTransitions
    , finalStates = [ x ++ y | x <- (finalStates dfa1), y <- (finalStates dfa2)] 
    , caption = caption
    }
    where fn1 = makeTransitionFunction (transitions dfa1) 
          fn2 = makeTransitionFunction (transitions dfa2) 
          interleaveTransitions = 
              [((q1 ++ q2), s, (q1' ++ q2)) | 
                  q1 <- states dfa1, q2 <- states dfa2, s <- alphabet dfa1, q1' <- fn1 (q1, s)] ++
              [((q1 ++ q2), s, (q1 ++ q2')) | 
                  q1 <- states dfa1, q2 <- states dfa2, s <- alphabet dfa2, q2' <- fn2 (q2, s)]
          

interleaveThreads :: DFA -> Int -> String -> GenericDFA [String] (String, Int)
interleaveThreads dfa 1 _ = addThreadIndexToDFA dfa 1 
interleaveThreads dfa n caption = interleave 
        (makeInterleavedProgram dfa (n - 1)) 
        (addThreadIndexToDFA dfa n)
        caption

makeInterleavedProgram :: DFA -> Int -> GenericDFA [String] (String, Int)
makeInterleavedProgram dfa n = interleaveThreads dfa n cap 
    where cap = "Interleaving " ++ (show n) ++ 
                " Copies of Thread Template \"" ++ 
                (caption dfa) ++ "\""
    
    