{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Artifact
    ( Artifact (..)
    , incRating
    , eitherDecode'
    , encode
    ) where

import Data.Aeson ( FromJSON, ToJSON (..)
                  , genericToJSON
                  , defaultOptions
                  , eitherDecode'
                  , encode
                  )
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Aeson instances
instance ToJSON Artifact where
    toJSON = genericToJSON defaultOptions
instance FromJSON Artifact where

-- | Data structure representing one artifact.
data Artifact
    = Artifact
      { storageUrl  :: !Text
      , resourceUrl :: !Text
      , rating      :: !Int
      }
    deriving (Generic, Show)

-- | Increase the rating one step. If the maximum rating is reached
-- no forther stepping is done.
incRating :: Artifact -> Artifact
incRating art@Artifact {..}
    | rating < 3 = art { rating = rating + 1 }
    | otherwise  = art
