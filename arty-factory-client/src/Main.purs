module Main where

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Exception (throwException)

import Halogen (HalogenEffects (), action, runUI)
import Halogen.Util (appendToBody, onLoad)

import ArtyFactory (AppEffects, Query (..), ui, initialState)

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
    {node: node, driver: driver} <- runUI ui initialState
    onLoad $ appendToBody node
    driver (action Refresh)

