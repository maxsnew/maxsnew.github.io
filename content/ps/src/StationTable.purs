module StationTable (TableData, Query(..), stationTable, mkTableData) where

import Prelude -- (type (~>), Void, const, map, pure, show, ($), (+), (-), (<$), (<<<), (<>), discard)

import Control.Monad.Aff (Aff)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import DOM (DOM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (pow)

import Common

type State = TableData
type TableData = { place :: Place
                 , stations :: Array ResolvedStation
                 , limit :: Int
                 , initLimit :: Int
                 }

data Query a = NewData (Array ResolvedStation) a
             | IncLimit a
             | ResetLimit a

mkTableData :: { place :: Place, stations :: Array ResolvedStation, initLimit :: Int } -> TableData
mkTableData s = { place: s.place, stations: s.stations, initLimit: s.initLimit, limit: s.initLimit }

type UI eff = H.Component HH.HTML
                          Query
                          TableData
                          Void
                          (Aff (dom :: DOM | eff))

stationTable :: forall eff. UI eff
stationTable =
  H.component
  { initialState: (\x -> x)
  , render
  , eval
  , receiver: const Nothing
  }
  where

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $ [ HH.h2_ [HH.text $ "Places near " <> st.place.name ]
              , HH.table_ [ header, renderStations nearbyStations ]
              , HH.button [ HP.title "more stations"
                          , HE.onClick $ HE.input_ IncLimit
                          ]
                          [ HH.text "more stations"]
              ]
              <>
              if st.limit /= st.initLimit
              then [ HH.button [ HP.title "hide"
                               , HE.onClick $ HE.input_ ResetLimit
                               ]
                               [ HH.text "hide"] ]
              else []
    where
    nearbyStations = Array.take st.limit <<< Array.sortWith proximityToPlace <<< Array.filter (\x -> x.status.is_installed) $ st.stations
    proximityToPlace dat = proximity st.place dat.info

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval = case _ of
    NewData newStations next -> do
      H.modify $ \st -> st { stations = newStations }
      pure next

    IncLimit next -> do
      H.modify $ \st -> st {limit = st.limit + 5 }
      pure next

    ResetLimit next -> do
      H.modify $ \st -> st { limit = st.initLimit }
      pure next

renderStations :: forall p i. Array ResolvedStation -> HH.HTML p i
renderStations dat =
  HH.tbody_ $ map renderStation dat

renderStation :: forall p i. ResolvedStation -> HH.HTML p i
renderStation s = HH.tr_ $ map (HH.td_ <<< pure <<< HH.text)
                  [ s.info.name
                  , show s.status.num_bikes_available
                  , show s.status.num_docks_available
                  ]

proximity :: forall r r'. Coordinates r -> Coordinates r' -> Number
proximity p1 p2 = pow (p1.lat - p2.lat) 2.0 + pow (p1.lon - p2.lon) 2.0

header :: forall p i. HH.HTML p i
header = HH.thead_ $ [ HH.tr_ $ map (HH.th_ <<< pure <<< HH.text) [ "Station", "Bikes", "Vacancies" ]]

