{-# LANGUAGE OverloadedStrings #-}
module Handler
    ( listArtifacts
    , uploadFile
    , voteUp
    , deleteFile
    ) where

--import Data.Text (Text)
import Network.Hive
import Pipes ((<-<), runEffect)

import Data.Text as T
import qualified Pipes.Prelude as P

import Artifact (Artifact (..))
import ArtyStore ( artifacts
                 , insertArtifact
                 , tryVoteUpArtifact
                 , tryInsertFilename
                 , tryDeleteArtifact
                 , mkFileWriter
                 )
import Context ( Context (..))
import ResourceId (mkResourceId)

-- | List all artifacts.
listArtifacts :: Context -> Handler HandlerResponse
listArtifacts context =
    respondJSON Ok =<< liftIO ( artifacts (artyStore context))

-- | Upload a new file to the store and create a new meta data entry
-- in the database.
uploadFile :: Context -> Handler HandlerResponse
uploadFile context = do
    maybeFile <- queryValue "file"
    case maybeFile of
        Just file -> do
          inserted <- liftIO $ tryInsertFilename file (artyStore context)
          if inserted then do transferFile file context
                              mkArtifact file context
                              respondJSON Created =<<
                                  liftIO (artifacts (artyStore context))
                      else respondText Conflict "File already exist"

        Nothing   -> respondText BadRequest "Missing file as query param"

-- | Stream the the body to the file storage using the file writer pipe.
transferFile :: Text -> Context -> Handler ()
transferFile file context = do
    stream <- bodyStream
    writer <- liftIO $ mkFileWriter (T.unpack file) (artyStore context)
    liftIO $ runEffect $ P.mapM_ writer <-< stream

-- | Make and insert an artifact into the meta store.
mkArtifact :: Text -> Context -> Handler ()
mkArtifact file context = do
    resourceId <- liftIO $ mkResourceId file
    let artifact =
          Artifact { storageUrl  = "/storage/" `mappend` file
                   , resourceUrl = "/artifact/" `mappend` resourceId
                   , rating      = 0
                   }
    liftIO $ insertArtifact resourceId artifact (artyStore context)

-- | Vote for the captured artifact.
voteUp :: Context -> Handler HandlerResponse
voteUp context = do
    artId <- capture "artId"
    found <- liftIO $ tryVoteUpArtifact artId (artyStore context)
    if found then listArtifacts context
             else respondText NotFound "Artifact not found"

-- | Delete an artifact and its associated file.
deleteFile :: Context -> Handler HandlerResponse
deleteFile context = do
    artId   <- capture "artId"
    deleted <- liftIO $ tryDeleteArtifact artId (artyStore context)
    if deleted then listArtifacts context
               else respondText NotFound "Artifact not found"

