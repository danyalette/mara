{-# LANGUAGE OverloadedStrings #-}

module DFADot where 

import Data.Graph.Inductive
import qualified Data.Text.Lazy as DTL
import Data.GraphViz
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Complete
import DFA 
import Data.List 

-- Convert DFA to Data.Graph.Inductive graph format
makeGraphFromDFA :: (Ord a, Show a, Show b) => GenericDFA a b -> Gr String String
makeGraphFromDFA (DFA states _ _ trans _ _) = run_ Data.Graph.Inductive.empty $
            do insMapNodesM (map show states)
               insMapEdgesM (mergeEdges (map (\(x, y, z) -> ((show x), (show z), (show y))) trans))

-- Merge edges that have the same source and destination, 
-- to de-clutter the diagram. 
mergeEdges :: [(String, String, String)] -> [(String, String, String)]
mergeEdges transitions = foldr (\t l -> combine (mergeWith l t)) [] transitions
  where combine (l, t) = t:l 
        mergeWith lst (x, y, z) = foldl fn ([], (x, y, z)) lst 
        fn (l, (x, y, z)) (x', y', z') = if (x == x' && y == y') 
            then (l, (x, y, z ++ ", " ++ z'))
            else ((x', y', z'):l, (x, y, z))

-- Convert DFA to DotGraph Format 
makeDotFromDFA :: (Show a, Eq a, Ord a, Show b) => GenericDFA a b -> Bool -> Maybe (a -> Int) -> DotGraph Node
makeDotFromDFA dfa cluster clusterFn= graphToDot params graph
  where graph :: Gr String String
        graph = makeGraphFromDFA dfa
        params = blankParams { globalAttributes = [
                                  GraphAttrs [toLabel $ "\n" ++ (caption dfa), style invis, Ratio (AspectRatio 0.5625)]
                                  ]
                         , clusterBy        = clustBy
                         , clusterID        = Num . Int
                         , isDotCluster     = const True
                         , fmtNode          = nodeFmt
                         , fmtEdge          = edgeFmt
                         , fmtCluster       = const []
                         , isDirected       = True
                         }   
        clustBy (n,l) = C (if cluster then (fn l) else 0) $ N (n,l)
          where fn = case clusterFn of 
                              Nothing -> approxClustString
                              Just f -> (\label -> case (getStateFromStateString label) of 
                                                      [] -> 0
                                                      x:xs -> f x
                                        )
        label :: String -> Attribute
        label l = toLabel $ "  " ++ l ++ "  "
        nodeFmt :: (a, String) -> Attributes
        nodeFmt (x, l) = if (any ( == l) (map show (finalStates dfa)))
            then (Peripheries 2):(nodeAttr l) -- Add double outline to final states
            else (nodeAttr l)
        edgeFmt (x, y, l) = [label l]
        nodeAttr l = if (l == (show (initialState dfa))) then [label ("--> " ++ l ++ "(initial)")] else [label l]
        getStateFromStateString string = filter (\state -> string == (show state)) (states dfa)

          

renderDFA dfa cluster clustFn = DTL.unpack $ renderDot $ toDot $ makeDotFromDFA dfa cluster clustFn

-- For clustering, I am using sorted state label as rough approx of 
-- state symmetry equivalence canonicalization function 
approxClustString :: String -> Int
approxClustString label = (read (foldl fn "" (sort label)))
    where fn acc c = acc ++ (show . (\x -> x `mod` 100) . fromEnum) c


