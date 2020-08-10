{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-} -- generic is used to infer toJson and fromJson 

module DFA where
  
import GHC.Generics

data GenericDFA a b = DFA {
      states :: [a] 
    , alphabet :: [b] 
    , initialState :: a 
    , transitions :: [(a, b, a)]  -- [(state, symbol, state)]
    , finalStates :: [a] 
    , caption :: String
  } deriving (Show, Generic)

type DFA = GenericDFA String String

in' :: (Eq a) => a -> [a] -> Bool
in' x l = any (== x) l

{-- verifyDFAConstraints checks if the following constraints are satisfied:
    - initialState ∈ states  
    - (q,s,q') ∈ transitions ⇒ q, q' ∈  states and s ∈ alphabet 
    - finalStates ⊆ states 
    - Note that I do not check if the transition relation is a function
    - so this graph might be an NFA. 
--}
verifyDFAConstraints :: (Eq a, Eq b) => GenericDFA a b -> Bool
verifyDFAConstraints dfa = checkInit && checkTransitions && checkFinals 
  where checkInit = (initialState dfa) `in'` (states dfa)
        checkTransition (q, s, q') = (q `in'` (states dfa)) && 
              (q' `in'` (states dfa)) && 
              (s `in'` (alphabet dfa))
        checkTransitions = all checkTransition (transitions dfa)
        checkFinals = all (\f -> f `in'`(states dfa)) (finalStates dfa)
