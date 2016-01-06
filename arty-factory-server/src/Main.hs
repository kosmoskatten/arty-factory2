{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Network.Hive

import Artifact (metaDataFP, readMetadata)
import Context (Context (..), newTVarIO)
import Handler (listArtifacts, voteUp)

main :: IO ()
main = do
    let dd = "/home/patrik/slask/data-dir"
    eMeta <- readMetadata $ metaDataFP dd
    case eMeta of
        Left err -> print err
        Right m  -> do
            context <- Context "site" dd <$> newTVarIO m
            runServer context 8888

runServer :: Context -> Int -> IO ()
runServer context serverPort = do
    let config = defaultHiveConfig { port = serverPort }
    hive config $ do
        match GET <!> None
                  ==> redirectTo "index.html"
        match GET </> "artifact" <!> None
                  ==> listArtifacts context
        match PUT </> "artifact" </:> "artId" </> "vote" <!> None
                  ==> voteUp context
        -- TODO: Check presedence for ==> etc.
        matchAll           ==> serveDirectory (siteDir context)
