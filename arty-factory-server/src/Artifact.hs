{-# LANGUAGE DeriveGeneric #-}
module Artifact
    ( Artifact (..)
    , ArtiMap
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

-- | Aeson instances
instance ToJSON Artifact where
    toJSON = genericToJSON defaultOptions
instance FromJSON Artifact where

type ArtiMap = Map Text Artifact

-- | Data structure representing one artifact.
data Artifact
    = Artifact
      { artId       :: !Text
      , resourceUrl :: !Text
      , rating      :: !Int
      }
    deriving (Generic, Show)

metaDataFP :: FilePath -> FilePath
metaDataFP dir = dir ++ "/" ++ "data.json"

writeMetadata :: FilePath -> ArtiMap -> IO ()
writeMetadata file = LBS.writeFile file . encode . Map.elems

readMetadata :: FilePath -> IO (Either String ArtiMap)
readMetadata file = do
    eData <- eitherDecode' <$> LBS.readFile file
    case eData of
      Right d -> do
        let convert = Map.fromList . toPairs
        return (Right $ convert d)
      Left  e -> return (Left e)

toPairs :: [Artifact] -> [(Text, Artifact)]
toPairs = map (\a -> (artId a, a))
