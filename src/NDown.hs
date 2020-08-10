{--
N-down (named for the latex expression $[N]^*_{\downarrow}$) is 
the language of all sequences of natural numbers 1-N that are lexicographically
minimal w.r.t. the symmetry equivalence relation of sequences of numbers. 
--}
module NDown where 

import DFA 

type NDown = GenericDFA Int Int

makeNDown :: Int -> NDown
makeNDown n = DFA {
      states = allStates
    , alphabet = [1..n]
    , initialState = state 0
    , transitions = 
          map (\i -> (state i, i + 1, state (i + 1))) [0..(n-1)] ++
          foldl (\l i -> (map (\j -> ((state i, j, state i))) [1..i]) ++ l) [] [1..n]
    , finalStates = allStates
    , caption = "N-down DFA for N=" ++ (show n)
    }
    where allStates = map state [0..n]
          state i = i -- or named states? 

