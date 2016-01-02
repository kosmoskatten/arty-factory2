{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Network.Hive

main :: IO ()
main =
    hive defaultHiveConfig $ do
        match GET <!> None ==> redirectTo "index.html"
        matchAll           ==> serveDirectory "site"
