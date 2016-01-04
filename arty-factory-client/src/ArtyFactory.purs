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
    = { resourceUrl :: String
      , rating      :: Int
      }

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
               , artifacts: [ { resourceUrl: "/storage/foo.tgz"
                              , rating: 0 
                              }
                            , { resourceUrl: "/storage/bar.tgz"
                              , rating: 2
                              }
                            ]
               }

-- | The UI for the ArtyFactory.
ui :: forall g. (Functor g) => Component State Query g
ui = component render eval

-- | The composition of the UI rendering.
render :: State -> ComponentHTML Query
render st = 
    H.div_ [ renderNavbar st
           , if st.page == Download then
                 renderDownloadPane st 
             else
                 renderUploadPane st
           ]

-- | Render the navigation bar.
renderNavbar :: State -> ComponentHTML Query
renderNavbar st =
    H.nav [ P.classes [ className "navbar"
                      , className "navbar-inverse"
                      , className "navbar-fixed-top"
                      ]
          ]

      [ H.div [ P.class_ (className "container-fluid") ]

          [ H.div [ P.class_ (className "navbar-header") ]
              [ H.a [ P.class_ (className "navbar-brand") , P.href "#" ]
                  [ H.text "Arty-Factory" ]
              ]

          , H.div_ $
              [ H.ul [ P.classes [ className "nav"
                                 , className "navbar-nav"
                                 ]
                     ]
                  renderLinks
              ] ++ maybeRenderRefresh
          ]
      ]
    where
      renderLinks :: Array (ComponentHTML Query)
      renderLinks =
          [ H.li (linkClass Download)
                 [ H.a [ P.href "#"
                       , E.onClick (E.input_ GotoDownload) 
                       ]
                       [ H.text "Download" ] 
                 ]

          , H.li (linkClass Upload)
                 [ H.a [ P.href "#"
                       , E.onClick (E.input_ GotoUpload) 
                       ]
                       [ H.text "Upload" ] 
              ]
          ]
      linkClass page =
          if page == st.page then
              [ P.class_ (className "active") ]
          else
              []
      maybeRenderRefresh :: Array (ComponentHTML Query)
      maybeRenderRefresh =
        if st.page == Download then
          [ H.ul [ P.classes [ className "nav"
                             , className "navbar-nav"
                             , className "navbar-right"
                             ]
                 ]
                 [ H.li_
                     [ H.a [ P.href "#" ]
                       [ H.span [ P.classes [ className "glyphicon"
                                             , className "glyphicon-refresh"
                                              ]
                                ]
                                []
                        ]
                     ]
                 ]
          ]
        else
          []

renderDownloadPane :: State -> ComponentHTML Query
renderDownloadPane st =
    -- TODO: Why is the offset needed?
    H.div [ P.classes [ className "container"
                      , className "offset" ]
          ]
      [ H.h2_ [ H.text "Available artifacts" ]
      , H.table [ P.classes [ className "table"
                            , className "table-striped"
                            ]
                ]
          [ H.thead_
              [ H.tr_ 
                  [ H.th_ [ H.text "URL" ]
                  , H.th_ [ H.text "Download" ]
                  , H.th_ [ H.text "Rating" ]
                  , H.th_ [ H.text "Vote Up" ]
                  ]
              ]
          , H.tbody_ $ map renderTableEntry st.artifacts
          ]
      ]

renderTableEntry :: Artifact -> ComponentHTML Query
renderTableEntry art =
    H.tr_
      [ H.td_ [ H.text art.resourceUrl ]
      -- | TODO: Add the download attribute in Halogen.
      , H.td_ [ H.a [ P.href art.resourceUrl ]
                    [ H.span [ P.classes [ className "glyphicon"
                                         , className "glyphicon-download"
                                         ]
                             , P.title $ "Download " ++ art.resourceUrl
                             ] []
                    ]
              ]
      , renderRating art.rating
      , H.td_ [ H.a [ P.href "#" ]
                    [ H.span [ P.classes [ className "glyphicon"
                                         , className "glyphicon-thumbs-up"
                                         ]
                             , P.title "Vote for artifact"
                             ] []
                    ]
              ]
      ]

renderRating :: Int -> ComponentHTML Query
renderRating 0 =
    H.td [ P.title "0 of 3" ] $ map renderStar [ "glyphicon-star-empty"
                                               , "glyphicon-star-empty"
                                               , "glyphicon-star-empty"
                                               ]
renderRating 1 =
    H.td [ P.title "1 of 3" ] $ map renderStar [ "glyphicon-star"
                                               , "glyphicon-star-empty"
                                               , "glyphicon-star-empty"
                                               ]
renderRating 2 =
    H.td [ P.title "2 of 3" ] $ map renderStar [ "glyphicon-star"
                                               , "glyphicon-star"
                                               , "glyphicon-star-empty"
                                               ]
renderRating _ =
    H.td [ P.title "3 of 3" ] $ map renderStar [ "glyphicon-star"
                                               , "glyphicon-star"
                                               , "glyphicon-star"
                                               ]

renderStar :: String -> ComponentHTML Query
renderStar star =
    H.span [ P.classes [ className "glyphicon"
                       , className star
                       ]
           ] []

renderUploadPane :: State -> ComponentHTML Query
renderUploadPane st =
    H.div [ P.classes [ className "container"
                      , className "offset" ]
          ]
          (map (\s -> H.p_ [H.text s]) ["Sigge", "Frasse", "Nisse"])
      --[ H.p_ [ H.text "Olleh" ]
      --, H.p_ [ H.text "Olleh" ]
      --, H.p_ [ H.text "Olleh" ]
      --]

eval :: forall g. (Functor g) => Natural Query (ComponentDSL State Query g)
eval (GotoDownload next) = do
    modify $ \st -> st { page = Download }
    pure next
eval (GotoUpload next) = do
    modify $ \st -> st { page = Upload }
    pure next
