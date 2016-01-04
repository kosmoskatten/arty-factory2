module Context
    ( Context (..)
    , newMVar
    ) where

import Control.Concurrent (MVar, newMVar)

import Artifact (ArtiMap)

data Context
    = Context
      { siteDir :: !FilePath
      , dataDir :: !FilePath
      , artiMap :: MVar ArtiMap
      }

