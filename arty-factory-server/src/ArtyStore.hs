{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ArtyStore
    ( ArtyStore
    , initFromDirectory
    , artifacts
    , insertArtifact
    , tryVoteUpArtifact
    , tryInsertFilename
    , tryDeleteArtifact
    , mkFileWriter
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM ( STM, TVar
                              , atomically
                              , modifyTVar
                              , newTVarIO
                              , readTVar
                              , readTVarIO
                              , writeTVar
                              )
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.Map.Lazy (Map)
import Data.Set (Set)
import Data.Text (Text)
import System.Directory ( createDirectoryIfMissing
                        , doesFileExist
                        , removeFile
                        )
import System.FilePath ((</>), takeFileName)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Artifact (Artifact (..), incRating, eitherDecode', encode)
import ResourceId (ResourceId)

type ArtyMap = Map ResourceId Artifact
type FileSet = Set Text

data ArtyStore
    = ArtyStore
      { storageDir :: !FilePath
      , metaStore  :: !FilePath
      , artyMap    :: TVar ArtyMap
      , fileSet    :: TVar FileSet
      , modified   :: TVar Bool
      }

-- | Initialize the meta store, with the base given by the file path
-- argument. If no meta file exist at initialization an empty metastore
-- will created, otherwise it will be created from the metastore.json.
initFromDirectory :: FilePath -> IO (Either String ArtyStore)
initFromDirectory baseDir = do
    let storageDir' = baseDir </> storageDirName
        metaStore'  = baseDir </> metaStoreFile

    createDirectoryIfMissing True storageDir'

    fileExist  <- doesFileExist metaStore'
    eArtifacts <- if fileExist then readMetaStore metaStore'
                               else return (Right [])

    case eArtifacts of
        Right xs -> do
            artyMap'  <- newTVarIO $ mkArtyMap xs
            fileSet'  <- newTVarIO $ mkFileSet xs
            modified' <- newTVarIO False
            let artyStore = ArtyStore
                            { storageDir = storageDir'
                            , metaStore  = metaStore'
                            , artyMap    = artyMap'
                            , fileSet    = fileSet'
                            , modified   = modified'
                            }
            void $ async (metaStoreWriter artyStore)
            return $ Right artyStore

        Left err -> return $ Left err

-- | List all the artifacts in the store.
artifacts :: ArtyStore -> IO [Artifact]
artifacts artyStore = Map.elems <$> readTVarIO (artyMap artyStore)

-- | Insert a new artifact to the store.
insertArtifact :: ResourceId -> Artifact -> ArtyStore -> IO ()
insertArtifact resId art ArtyStore {..} =
    atomically $ do
      modifyTVar artyMap $ Map.insert resId art
      writeTVar modified True

-- | Try vote up the given artifact. If found the artifact is voted for
-- and the value of True is returned. Otherwise the value of False is
-- returned.
tryVoteUpArtifact :: Text -> ArtyStore -> IO Bool
tryVoteUpArtifact artId ArtyStore {..} = atomically go
  where
    go :: STM Bool
    go = do
      artyMap' <- readTVar artyMap
      if Map.member artId artyMap'
         then do writeTVar artyMap $
                     Map.update (Just . incRating) artId artyMap'
                 writeTVar modified True
                 return True
         else return False

-- | Try insert a filename into the fileset. If the name already is present
-- the operation will return False. If the name not is present it will
-- be inserted and True is returned.
tryInsertFilename :: Text -> ArtyStore -> IO Bool
tryInsertFilename file ArtyStore {..} = atomically go
  where
    go :: STM Bool
    go = do
      fileSet' <- readTVar fileSet
      if Set.notMember file fileSet'
         then do writeTVar fileSet $ Set.insert file fileSet'
                 return True
         else return False

-- | Try to delete an artifact and its associated storage. If the resource
-- is found the value of True is returned, otherwise False.
tryDeleteArtifact :: ResourceId -> ArtyStore -> IO Bool
tryDeleteArtifact artId ArtyStore {..} = do
    mArtifact <- atomically go
    case mArtifact of
        Just artifact -> do
            let fullPath = storageDir </>
                              takeFileName (T.unpack $ storageUrl artifact)
            removeFile fullPath
            return True
        Nothing       -> return False
    where
      go :: STM (Maybe Artifact)
      go = do
        artyMap' <- readTVar artyMap
        case Map.lookup artId artyMap' of
            Just artifact -> do
                writeTVar artyMap $ Map.delete artId artyMap'
                writeTVar modified True
                modifyTVar fileSet $
                  Set.delete (takeFileNameText $ storageUrl artifact)
                return (Just artifact)
            Nothing       -> return Nothing

-- | Make a file writer function which is carrying the file path in its
-- closure.
mkFileWriter :: FilePath -> ArtyStore -> IO (ByteString -> IO ())
mkFileWriter file ArtyStore {..} = do
    let fullPath = storageDir </> file
    BS.writeFile fullPath BS.empty
    return $ BS.appendFile fullPath

-- | Peridically every 5s check if the meta store needs to be written to
-- its backup store. Write if needed.
metaStoreWriter :: ArtyStore -> IO ()
metaStoreWriter artyStore =
    forever $ do
        threadDelay 5000000
        maybeMap <- atomically $ toggleModifiedFlag artyStore
        maybe (return ()) (writeMetaStore $ metaStore artyStore) maybeMap

-- | Atomically checking if the modified flag is set. If so, toggle the
-- flag and return the artymap copy for this version. If the flag not is
-- set Nothing is returned.
toggleModifiedFlag :: ArtyStore -> STM (Maybe ArtyMap)
toggleModifiedFlag artyStore = do
    modified' <- readTVar $ modified artyStore
    if modified' then do artyMap' <- readTVar $ artyMap artyStore
                         writeTVar (modified artyStore) False
                         return $ Just artyMap'
                 else return Nothing

readMetaStore :: FilePath -> IO (Either String [Artifact])
readMetaStore file = eitherDecode' <$> LBS.readFile file

writeMetaStore :: FilePath -> ArtyMap -> IO ()
writeMetaStore file = LBS.writeFile file . encode . Map.elems

-- | Make an arty-map from a list of artifacts.
mkArtyMap :: [Artifact] -> ArtyMap
mkArtyMap =
    Map.fromList . map (\art -> (lastSegment $ resourceUrl art, art))

-- | Make a set of the existing file names from a list of artifacts.
mkFileSet :: [Artifact] -> FileSet
mkFileSet = Set.fromList . map (lastSegment . storageUrl)

-- | Get the stuff after the last / (if any /).
lastSegment :: Text -> Text
lastSegment = last . T.splitOn "/"

metaStoreFile :: FilePath
metaStoreFile = "metastore.json"

storageDirName :: FilePath
storageDirName = "storage"

takeFileNameText :: Text -> Text
takeFileNameText = T.pack . takeFileName . T.unpack
