module App (State, Query(..), ui, statusUrl) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Argonaut.Parser as Arg
import Data.Either
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.Event (Event, preventDefault)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

import GBFS as GBFS

type State =
  { hubwayData :: Either String (GBFS.GbfsData (Array GBFS.StationStatus))
  }

data Query a
    = Refresh a

type UI eff = H.Component HH.HTML -- ^ what we're rendering to
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
  initialState s = { hubwayData: Left "loading..." }

  render :: State -> H.ComponentHTML Query
  render st =
      case st.hubwayData of
        Left e ->
            HH.text e
        Right hw ->
            HH.p_ [ HH.text $ show (map (_.station_id) hw.data') ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM, ajax :: AX.AJAX | eff))
  eval = case _ of
    Refresh next -> do
      response <- H.liftAff $ AX.get statusUrl
      H.modify (_ { hubwayData = parseResponse (response.response) })
      pure next

  parseResponse s = do
    js <- Arg.jsonParser s
    GBFS.parseStationStatuses js

statusUrl :: String
statusUrl = "https://gbfs.thehubway.com/gbfs/en/station_status.json"

infoUrl :: String
infoUrl = "https://gbfs.thehubway.com/gbfs/en/station_information.json"
