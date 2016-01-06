{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler
    ( listArtifacts
    , voteUp
    ) where

import Network.Hive

import Artifact (Artifact (..), member, toList, update)
import Context ( Context (..)
               , atomically
               , readTVar
               , readTVarIO
               , writeTVar
               )
import Data.Text (Text)

-- | List all artifacts.
listArtifacts :: Context -> Handler HandlerResponse
listArtifacts context = do
    artifacts <- toList <$> (liftIO $ readTVarIO (artiMap context))
    respondJSON Ok artifacts

-- | Vote for the captured artifact.
voteUp :: Context -> Handler HandlerResponse
voteUp context = do
    artId <- capture "artId"
    voted <- liftIO $ votedUp artId context
    if voted then listArtifacts context
             else respondText NotFound "Artifact not found"

-- | Atomic voting up. True if artifact was found in map, False otherwise.
votedUp :: Text -> Context -> IO Bool
votedUp artId context =
  atomically $ do
    artiMap' <- readTVar $ artiMap context
    if member artId artiMap'
       then do
         writeTVar (artiMap context) $ update (Just . satInc) artId artiMap'
         return True
       else
         return False

-- | Saturated increase of rating. Rating can be max three.
satInc :: Artifact -> Artifact
satInc art@Artifact {..}
    | rating < 3 = art { rating = rating + 1 }
    | otherwise  = art
