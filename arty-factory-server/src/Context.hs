module Context
    ( Context (..)
    , module Control.Concurrent.STM
    ) where

import Control.Concurrent.STM

import Artifact (ArtiMap)

data Context
    = Context
      { siteDir :: !FilePath
      , dataDir :: !FilePath
      , artiMap :: TVar ArtiMap
      }

