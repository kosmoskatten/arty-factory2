module ArtyFactory 
    ( Artifact
    , Page
    , State
    , Query
    , initialState
    , ui
    ) where

import Prelude

import Halogen
import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

data Page = Download | Upload

instance eqPage :: Eq Page where
    eq Download Download = true
    eq Upload Upload     = true
    eq _ _               = false

-- | The record for one artifact.
type Artifact
    = { resourceUrl :: String }

-- | The state for the Arty-Factory.
type State
    = { page      :: Page
      , artifacts :: Array Artifact
      }

-- | The query algebra for the ArtyFactory.
data Query a
    = GotoDownload a
    | GotoUpload a

-- | The initial - empty - state for the ArtyFactory.
initialState :: State
initialState = { page: Download
               , artifacts: [ { resourceUrl: "/storage/foo.tgz" }
                            , { resourceUrl: "/storage/bar.tgz" }
                            ]
               }

-- | The UI for the ArtyFactory.
ui :: forall g. (Functor g) => Component State Query g
ui = component render eval

-- | The composition of the UI rendering.
render :: State -> ComponentHTML Query
render st = H.div_ [ renderNavbar st, renderDownloadPane st ]

-- | Render the navigation bar.
renderNavbar :: State -> ComponentHTML Query
renderNavbar st =
    H.nav
      [ P.classes [ className "navbar"
                  , className "navbar-inverse"
                  , className "navbar-fixed-top"
                  ]
      ]
      [ H.div
          [ P.class_ (className "container-fluid") ]
          [ H.div
              [ P.class_ (className "navbar-header") ]
              [ H.a
                  [ P.class_ (className "navbar-brand") , P.href "#" ]
                  [ H.text "Arty-Factory" ]
              ]
          , H.div_
              [ H.ul
                  [ P.classes [ className "nav"
                              , className "navbar-nav"
                              ]
                  ]
                  renderLinks
              ]
          ]
      ]
    where
      renderLinks :: Array (ComponentHTML Query)
      renderLinks =
          [ H.li (linkClass Download)
              [ H.a 
                  [ P.href "#"
                  , E.onClick (E.input_ GotoDownload) ]
                  [ H.text "Download" ] 
              ]
          , H.li (linkClass Upload)
              [ H.a 
                  [ P.href "#"
                  , E.onClick (E.input_ GotoUpload) ]
                  [ H.text "Upload" ] 
              ]
          ]
      linkClass page =
          if page == st.page then
              [ P.class_ (className "active") ]
          else
              []

renderDownloadPane ::State -> ComponentHTML Query
renderDownloadPane st =
    H.div
      [ P.id_ "download", P.class_ (className "container-fluid") ]
      [ H.p_ [ H.text "Hello" ]
      ]

eval :: forall g. (Functor g) => Natural Query (ComponentDSL State Query g)
eval (GotoDownload next) = do
    modify $ \st -> st { page = Download }
    pure next
eval (GotoUpload next) = do
    modify $ \st -> st { page = Upload }
    pure next
