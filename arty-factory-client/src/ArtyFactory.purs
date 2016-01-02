module ArtyFactory 
    ( Artifact
    , State
    , Query
    , initialState
    , ui
    ) where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

-- | The record for one artifact.
type Artifact
    = { resourceUrl :: String }

-- | The state for the Arty-Factory.
type State
    = { artifacts :: Array Artifact }

-- | The query algebra for the ArtyFactory.
data Query a
    = Dummy a

-- | The initial - empty - state for the ArtyFactory.
initialState :: State
initialState = { artifacts: [ { resourceUrl: "/storage/foo.tgz" }
                            , { resourceUrl: "/storage/bar.tgz" }
                            ]
               }

-- | The UI for the ArtyFactory.
ui :: forall g. (Functor g) => Component State Query g
ui = component render eval

render :: State -> ComponentHTML Query
render st =
    H.p_ [ H.text "Hej hopp" ]

eval :: forall g. (Functor g) => Natural Query (ComponentDSL State Query g)
eval (Dummy next) = pure next
