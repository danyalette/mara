module TraceReduce where 

import DFA 
import NDown

traceReduceDFA :: GenericDFA [String] (String, Int) -> Int -> GenericDFA [String] (String, Int)
traceReduceDFA dfa n = DFA {
      states = [x ++ [show y] | x <- (states dfa), y <- [0..n]]
    , alphabet = alphabet dfa 
    , initialState = initialState dfa ++ [show 0]
    , transitions = [
          (q ++ [show r], (s, i), q' ++ [show r']) | 
          (q, (s, i), q') <- (transitions dfa), 
          (r, t, r') <- (transitions ndown),
          t == i
          ]
    , finalStates = [x ++ [show y] | x <- (finalStates dfa), y <- [0..n]]
    , caption = "Trace-based Symmetry Reduction of " ++ (caption dfa)
    }
    where ndown = makeNDown n