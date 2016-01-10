module ResourceId
    ( ResourceId
    , mkResourceId
    ) where

import Data.Hashable (hash, hashWithSalt)
import Data.Text (Text)
import Data.Time (getCurrentTime)

import qualified Data.Text as T

type ResourceId = Text

-- | Make a unique ResourceId for the given text string.
mkResourceId :: Text -> IO ResourceId
mkResourceId text = do
    now <- getCurrentTime
    let salt = hash (show now)
    return $ conv (hashWithSalt salt text)
      where
        conv :: Int -> ResourceId
        conv = T.pack . show . abs
