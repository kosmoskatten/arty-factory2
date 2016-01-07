module ArtyFactory 
    ( AppEffects
    , Artifact
    , Page
    , State
    , Query (..)
    , initialState
    , ui
    ) where

import Prelude

import Control.Monad.Aff (Aff ())
import Data.Either (Either (..))
import Data.Foreign
import Data.Foreign.Class

import Halogen ( ComponentHTML
               , ComponentDSL
               , HalogenEffects
               , Component, Natural
               , component
               , modify
               , liftAff'
               )
import Halogen.HTML.Core (className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Properties.Indexed as P

import Network.HTTP.Affjax (AJAX (), get, put)

--import Artifact (Artifact (..))

-- | Record to describe one artifact.
data Artifact
    = Artifact
      { resourceUrl :: String
      , storageUrl  :: String
      , rating      :: Int
      }

-- | IsForeign instance.
instance foreignArtifact :: IsForeign Artifact where
    read value = do
        resourceUrl <- readProp "resourceUrl" value
        storageUrl  <- readProp "storageUrl" value
        rating      <- readProp "rating" value
        return $ Artifact 
              { resourceUrl: resourceUrl
              , storageUrl: storageUrl
              , rating: rating 
              }

type AppEffects eff = HalogenEffects (ajax :: AJAX | eff)

data Page = Download | Upload

instance eqPage :: Eq Page where
    eq Download Download = true
    eq Upload Upload     = true
    eq _ _               = false

-- | The state for the Arty-Factory.
type State
    = { page      :: Page
      , artifacts :: Array Artifact
      }

-- | The query algebra for the ArtyFactory.
data Query a
    = GotoDownload a
    | GotoUpload a
    | Refresh a
    | VoteUp String a

-- | The initial - empty - state for the ArtyFactory.
initialState :: State
initialState = { page: Download
               , artifacts: []
               }

-- | The UI for the ArtyFactory.
ui :: forall eff. Component State Query (Aff (AppEffects eff))
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
                     [ H.a [ P.href "#"
                           , E.onClick (E.input_ Refresh) 
                           ]
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
renderTableEntry (Artifact art) =
    H.tr_
      [ H.td_ [ H.text art.storageUrl ]
      -- | TODO: Add the download attribute in Halogen.
      , H.td_ [ H.a [ P.href art.storageUrl ]
                    [ H.span [ P.classes [ className "glyphicon"
                                         , className "glyphicon-download"
                                         ]
                             , P.title $ "Download " ++ art.storageUrl
                             ] []
                    ]
              ]
      , renderRating art.rating
      , H.td_ [ H.a [ P.href "#"
                    , E.onClick (E.input_ (VoteUp art.resourceUrl))
                    ]
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

eval :: forall eff. Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
eval (GotoDownload next) = do
    modify $ \st -> st { page = Download }
    pure next
eval (GotoUpload next) = do
    modify $ \st -> st { page = Upload }
    pure next
eval (Refresh next) = do
    result <- liftAff' refreshArtifacts
    case result of
        Right artifacts -> do
          modify $ \st -> st { artifacts = artifacts }
          pure next
        _               -> pure next
eval (VoteUp resourceUrl next) = do
    result <- liftAff' (voteUpArtifact resourceUrl)
    case result of
        Right artifacts -> do
          modify $ \st -> st { artifacts = artifacts }
          pure next
        _               -> pure next

refreshArtifacts :: forall eff. Aff (ajax :: AJAX | eff) 
                        (Either String (Array Artifact))
refreshArtifacts = do
    result <- get "/artifact"
    let reply = result.response
    case readJSON reply of
        Right artifacts -> return (Right artifacts)
        Left _          -> return (Left "An error occurred")

voteUpArtifact :: forall eff. String
               -> Aff (ajax :: AJAX | eff) (Either String (Array Artifact))
voteUpArtifact resourceUrl = do
    result <- put (resourceUrl ++ "/vote") ""
    let reply = result.response
    case readJSON reply of
        Right artifacts -> return (Right artifacts)
        Left _          -> return (Left "An error occured")

