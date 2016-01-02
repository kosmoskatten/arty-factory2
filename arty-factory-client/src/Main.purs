module Main where

import Prelude
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff ())
import Control.Monad.Eff.Exception (throwException)

import Halogen (HalogenEffects (), runUI)
import Halogen.Util (appendToBody, onLoad)

import ArtyFactory (ui, initialState)

main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
    app <- runUI ui initialState
    onLoad $ appendToBody app.node

