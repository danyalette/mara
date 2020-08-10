
module Parser where 

import Options.Applicative
import Data.Semigroup ((<>))

data Opts = Opts
  { filePath    :: String
  , threadCount :: Int 
  , cluster     :: Bool
  , traceReduce :: Bool
  , reachReduce :: Bool}
  
parser :: Parser Opts 
parser = Opts
          <$> strOption
              ( long "file"
              <> short 'f'
             <> metavar "FILEPATH"
             <> help "Path to thread template file." )
           <*> option auto
               ( long "threadcount"
              <> short 'n'
              <> help "Number of threads."
              <> showDefault
              <> value 1
              <> metavar "INT" )
          <*> switch
              ( long "cluster"
             <> short 'c'
             <> help "Whether to cluster states." )
           <*> switch
               ( long "tracereduce"
              <> short 't'
              <> help "Whether to apply trace reduction." )
          <*> switch
              ( long "reachreduce"
             <> short 'r'
             <> help "Whether to apply reachability reduction." )