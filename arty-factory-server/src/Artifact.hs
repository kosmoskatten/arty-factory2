{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Artifact
    ( Artifact (..)
    , ArtiMap
    , member
    , toList
    , update
    , metaDataFP
    , writeMetadata
    , readMetadata
    ) where

import Data.Aeson ( FromJSON, ToJSON (..)
                  , genericToJSON
                  , defaultOptions
                  , eitherDecode'
                  , encode
                  )
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T

-- | Aeson instances
instance ToJSON Artifact where
    toJSON = genericToJSON defaultOptions
instance FromJSON Artifact where

type ArtiMap = Map Text Artifact

-- | Data structure representing one artifact.
data Artifact
    = Artifact
      { storageUrl  :: !Text
      , resourceUrl :: !Text
      , rating      :: !Int
      }
    deriving (Generic, Show)

member :: Text -> ArtiMap -> Bool
member = Map.member

toList :: ArtiMap -> [Artifact]
toList = Map.elems

update :: (Artifact -> Maybe Artifact) -> Text -> ArtiMap -> ArtiMap
update = Map.update

metaDataFP :: FilePath -> FilePath
metaDataFP dir = dir ++ "/" ++ "data.json"

writeMetadata :: FilePath -> ArtiMap -> IO ()
writeMetadata file = LBS.writeFile file . encode . toList

readMetadata :: FilePath -> IO (Either String ArtiMap)
readMetadata file = do
    eData <- eitherDecode' <$> LBS.readFile file
    case eData of
      Right d -> do
        let convert = Map.fromList . toPairs
        return (Right $ convert d)
      Left  e -> return (Left e)

toPairs :: [Artifact] -> [(Text, Artifact)]
toPairs = map (\a -> (last $ T.splitOn "/" (resourceUrl a), a))
