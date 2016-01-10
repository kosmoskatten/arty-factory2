module Context
    ( Context (..)
    ) where

import ArtyStore (ArtyStore)

data Context
    = Context
      { siteDir   :: !FilePath
      , storeDir  :: !FilePath
      , artyStore :: !ArtyStore
      }

