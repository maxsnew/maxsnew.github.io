module App (State, Query(..), ui, statusUrl) where

import Prelude (type (~>), Void, bind, const, discard, map, negate, pure, show,
                ($), (+), (-), (<$>), (<*>), (<<<), (<>))
import Control.Monad.Aff (Aff)
import Data.Argonaut.Core as Arg
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.Traversable (foldMap)
import DOM (DOM)
import Math (pow)

import Halogen as H
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
-- import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

import GBFS as GBFS

type State = Maybe
  { stationStatuses :: Either String (Array GBFS.StationStatus)
  , stationInfos :: Either String (Array GBFS.StationInformation)
  }

-- invariant: info.station_id == status.station_id
type ResolvedStation = {info :: GBFS.StationInformation, status :: GBFS.StationStatus }
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
          case Tuple <$> hw.stationStatuses <*> hw.stationInfos of
            Left err -> HH.text ("Error: " <> err)
            Right (Tuple statuses infos) ->
              let hwData = mergeHWData { info: infos, status: statuses }
              in HH.div_ [ renderNearPlace hwData home, renderNearPlace hwData work ]

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
      js <- jsonParser s
      (_.data') <$> parser js

proximity :: forall r r'. Coordinates r -> Coordinates r' -> Number
proximity p1 p2 = pow (p1.lat - p2.lat) 2.0 + pow (p1.lon - p2.lon) 2.0

header :: forall p i. HH.HTML p i
header = HH.thead_ $ [ HH.tr_ $ map (HH.th_ <<< pure <<< HH.text) [ "Station", "Bikes", "Vacancies" ]]

renderNearPlace :: forall p i. Array ResolvedStation -> Place -> HH.HTML p i
renderNearPlace hwData place =
  HH.div_ [ HH.h2_ [HH.text $ "Places near " <> place.name ]
          , HH.table_ [ header, renderData nearbyData ]
          ]
  where
    nearbyData = Array.take 6 $ Array.sortWith (\dat -> proximity place dat.info) hwData

renderData :: forall p i. Array ResolvedStation -> HH.HTML p i
renderData dat =
  HH.tbody_ $ map renderStation dat

renderStation :: forall p i. ResolvedStation -> HH.HTML p i
renderStation s = HH.tr_ $ map (HH.td_ <<< pure <<< HH.text)
                  [ s.info.name
                  , show s.status.num_bikes_available
                  , show s.status.num_docks_available
                  ]


mergeHWData :: {info :: Array GBFS.StationInformation, status :: Array GBFS.StationStatus}
               -> Array ResolvedStation
mergeHWData is = foldMap findStatus statuses
  where
    infos = is.info
    statuses = is.status
    idMap :: SM.StrMap GBFS.StationInformation
    idMap = SM.unions $ map (\s -> SM.singleton s.station_id s) infos

    findStatus :: GBFS.StationStatus -> Array ResolvedStation
    findStatus status = case SM.lookup status.station_id idMap of
      Nothing -> mempty
      Just info  -> pure {info: info, status: status}

-- | Constants

statusUrl :: String
statusUrl = "https://gbfs.thehubway.com/gbfs/en/station_status.json"

infoUrl :: String
infoUrl = "https://gbfs.thehubway.com/gbfs/en/station_information.json"

home :: Place
home = { name : "home", lat : 42.34852014581272, lon : -71.13394662737846 }

work :: Place
work = { name : "work", lat : 42.33863, lon : -71.092228 }
