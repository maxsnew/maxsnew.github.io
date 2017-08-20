module App (State, Query(..), ui, statusUrl) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Argonaut as Arg
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
import Math (pow)

import GBFS as GBFS

type State = Maybe
  { stationStatuses :: Either String (Array GBFS.StationStatus)
  , stationInfos :: Either String (Array GBFS.StationInformation)
  }

type Coordinates r = { lat :: Number, lon :: Number | r }
type Place = Coordinates (name :: String)

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
  initialState s = Nothing

  render :: State -> H.ComponentHTML Query
  render st =
      case st of
        Nothing -> HH.text "Loading..."
        Just hw ->
            HH.p_ [ HH.text $ show (map (map GBFS.NTStationStatus) hw.stationStatuses)
                  , HH.text $ show (map (map GBFS.NTStationInformation) hw.stationInfos)
                  ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM, ajax :: AX.AJAX | eff))
  eval = case _ of
    Refresh next -> do
      infos <- H.liftAff $ getParse GBFS.parseStationInfos infoUrl
      statuses <- H.liftAff $ getParse GBFS.parseStationStatuses statusUrl
      H.put (Just { stationStatuses: statuses, stationInfos: infos })
      pure next

  getParse :: forall a e. (Arg.Json -> Either String (GBFS.GbfsData a)) -> String -> Aff (ajax :: AX.AJAX | e) (Either String a)
  getParse parser url = do
    resp <- AX.get url
    pure $ parseResponse (resp.response)
    where
    parseResponse s = do
      js <- Arg.jsonParser s
      (_.data') <$> parser js



statusUrl :: String
statusUrl = "https://gbfs.thehubway.com/gbfs/en/station_status.json"

infoUrl :: String
infoUrl = "https://gbfs.thehubway.com/gbfs/en/station_information.json"

home :: Place
home = { name : "home", lat : 42.34852014581272, lon : -71.13394662737846 }

distance :: forall r r'. Coordinates r -> Coordinates r' -> Number
distance p1 p2 = pow (p1.lat - p2.lat) 2.0 + pow (p1.lon - p2.lon) 2.0

