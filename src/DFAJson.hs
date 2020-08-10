{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module DFAJson where 

import Data.Aeson
import DFA
import qualified Data.ByteString.Lazy as B

instance FromJSON DFA

parseFile :: FilePath -> IO (Maybe DFA)
parseFile file = do
  x <- decodeFileStrict file :: IO (Maybe DFA)
  return x
