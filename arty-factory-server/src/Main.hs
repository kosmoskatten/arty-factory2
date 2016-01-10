{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Network.Hive

import ArtyStore (initFromDirectory)
import Context (Context (..))
import Handler (listArtifacts, uploadFile, voteUp)

main :: IO ()
main = do
    eArtyStore <- initFromDirectory "/home/patrik/slask/arty"
    case eArtyStore of
        Right artyStore' -> do
            let context = Context
                          { siteDir   = "site"
                          , storeDir  = "/home/patrik/slask/arty"
                          , artyStore = artyStore'
                          }
            runServer context 8888

        Left err         -> putStrLn $ "Error: " ++ err

runServer :: Context -> Int -> IO ()
runServer context serverPort = do
    let config = defaultHiveConfig { port = serverPort }
    hive config $ do

        -- Rule 1. Redirect all GETs on the root to index.html.
        match GET  <!> None
                   ==> redirectTo "index.html"

        -- Rule 2. GET on /artifact shall give all artifacts as JSON
        -- and 200/OK.
        match GET  </> "artifact" <!> None
                   ==> listArtifacts context

        -- Rule 3. POST on /artifact will create a new entry. Respond with
        -- 201/Created and all artifacts as JSON. Alternatively
        -- 400/Bad Request if "?file=<filename>" is missing or
        -- 409/Conflict if file already is uploaded.
        match POST </> "artifact" <!> None
                   ==> uploadFile context

        -- Rule 4. PUT on /artifact/<resource id>/vote will vote an entry.
        -- Respond with 200/OK and all artifacts as JSON. Alternatively
        -- 404/Not Found if the resource don't exist.
        match PUT  </> "artifact" </:> "artId" </> "vote" <!> None
                   ==> voteUp context

        -- Rule 5. GET on a storage item in the storeDir.
        match GET  </> "storage" </:> "artId" <!> None
                   ==> serveDirectory (storeDir context)

        -- Rule 6. Default rule which will serve the siteDir.
        matchAll           ==> serveDirectory (siteDir context)
