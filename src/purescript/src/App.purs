module App (State, Query(..), ui, statusUrl) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

type State =
  { hubwayData :: Maybe String
  }

data Query a
    = MakeRequest String a

type UI eff = H.Component HH.HTML -- ^ what we're rendering too
                          Query   -- ^ Messages
                          String  -- ^ Initial Data
                          Void    -- ^ idk
                          (Aff (dom :: DOM, ajax :: AX.AJAX | eff))

ui :: forall eff. UI eff
ui =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: String -> State
  initialState s = { hubwayData: Nothing }

  mkButton s = 
            HH.button
                  [ HP.title s
                  , HE.onClick $ HE.input_ (MakeRequest s)
                  ]
                  [ HH.text s
                  ]     
  render :: State -> H.ComponentHTML Query
  render st =
      case st.hubwayData of
        Nothing ->
            HH.body [ HE.onLoad $ HE.input_ (MakeRequest statusUrl)]
                    [ HH.text "Loading..." ]
        Just hw ->
            HH.p_ [ HH.text hw ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM, ajax :: AX.AJAX | eff))
  eval = case _ of
    MakeRequest s next -> do
      response <- H.liftAff $ AX.get s
      H.modify (_ { hubwayData = Just response.response })
      pure next

statusUrl :: String
statusUrl = "https://gbfs.thehubway.com/gbfs/en/station_status.json"
