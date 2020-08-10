{-# LANGUAGE
      RecordWildCards
#-}
  
module Main where

import DFA
import DFADot (renderDFA)
import DFAJson (parseFile)
import Control.Monad (when)
import Interleaving (makeInterleavedProgram) 
import NDown 
import TraceReduce (traceReduceDFA)
import ReachableReduce (reachableReduceDFA)
import Data.List
import qualified Text.Read as TR 
import Parser (Opts (..), parser)
import Options.Applicative
import Data.Semigroup ((<>))


main :: IO ()
main = prog =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
     
prog Opts{..} = do 
  maybeDFA <- parseFile filePath
  case maybeDFA of 
    Nothing -> putStrLn "Error: Can't read JSON file"
    Just dfa -> if verifyDFAConstraints dfa 
        then do 
            let prog' = makeInterleavedProgram dfa threadCount
                prog = case (traceReduce, reachReduce) of 
                        (True, True)   -> reachableReduceDFA (traceReduceDFA prog' threadCount)
                        (True, False)  -> traceReduceDFA prog' threadCount
                        (False, True)  -> reachableReduceDFA prog'
                        (False, False) -> prog' 
            putStrLn $ renderDFA prog cluster (Just (clustId prog))
        else putStrLn "Error: Your DFA is invalid. Ensure that your list of states and alphabet are complete."
        where --- heuristic clustering
              clustMap :: GenericDFA [String] b -> [[String]]
              clustMap prog = foldl (\l state -> if (elem (sortS state) l) then l else (sortS state):l) [] (states prog)
              filterState :: [String] -> [String]
              filterState state = filter fn state
                where fn :: String -> Bool
                      fn s = case (TR.readMaybe s) :: Maybe Int of
                                Nothing -> True 
                                _ -> False
              findNum state = case (find fn state) of
                  Nothing -> 1
                  Just x -> (read x) + 2
                where fn :: String -> Bool
                      fn s = case (TR.readMaybe s) :: Maybe Int of
                                Nothing -> False 
                                _ -> True
              clustId :: GenericDFA [String] b -> [String] -> Int
              clustId prog state = case (elemIndex (sortS state) (clustMap prog)) of  
                                Nothing ->  -100 + (findNum state) * 10
                                Just i -> (i + 1) * 100 + (findNum state) * 10
              sortS = sort . filterState

