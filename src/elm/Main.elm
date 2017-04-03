import Dict
import Dict exposing (Dict)
import Html exposing (Html)
import Html.App as App
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode exposing ((:=), at, Decoder)
import Task

-- | Displays the current bike capacity for the hubway stations I care about

-- | Constants

type alias ID   = String
type alias Name = String
-- https://gbfs.thehubway.com/gbfs/en/station_information.json
-- could do this more automatically
stations_i_care_about : Dict ID Name
stations_i_care_about = Dict.fromList [ ("5", "Northeastern North Parking Lot")
                                      , ("160", "Wentworth")
                                      , ("66", "Griggs Street")
                                      , ("103", "JFK at Harvard Street")
                                      , ("41", "Packard's Corner")
                                      , ("8", "Brighton & Cambridge")
                                      , ("12", "Ruggles")]

main =
  App.program { init = init, view = view, update = update , subscriptions = \_ -> Sub.none }


-- MODEL

type alias StationStatus name = { station_id : name
                           , num_bikes_available : Int
                           -- , num_bikes_disabled : Int
                           , num_docks_available : Int
                           -- , num_docks_disabled : Int
                           -- , is_installed : Boolean
                           -- , is_renting : Boolean
                           -- , is_returning : Boolean
                           -- , last_reported : String -- Time
                           -- , eightd_has_available_keys : Boolean
                           }
type State = Loading
           | Fail Http.Error
           | Stations (List (StationStatus ID))

mk_station_status : name -> Int -> Int -> StationStatus name
mk_station_status id bikes docks =
    { station_id = id
    , num_bikes_available = bikes
    , num_docks_available  = docks
    }

station_status : Decoder (StationStatus ID)
station_status = Decode.object3 mk_station_status
                 ("station_id" := Decode.string)
                 ("num_bikes_available" := Decode.int)
                 ("num_docks_available" := Decode.int)

model_decoder : Decoder (List (StationStatus ID))
model_decoder = at ["data", "stations"] (Decode.list station_status)

fetch : Cmd Msg
fetch = Task.perform  Err Ok <|
        Http.get model_decoder "https://gbfs.thehubway.com/gbfs/en/station_status.json"
    
init : (State, Cmd Msg)
init = (Loading, fetch)

-- UPDATE

type alias Msg = Result Http.Error (List (StationStatus ID))

update : Msg -> State -> (State, Cmd Msg)
update new_info st =
    case new_info of
        Err e -> (Fail e, Cmd.none)
        Ok ss -> (Stations ss, Cmd.none)

-- VIEW

view_station : StationStatus Name -> List (Html a)
view_station station =
    List.map (Html.td [] << (\x -> [x]))
    [ Html.text (station.station_id)
    , Html.text (toString station.num_bikes_available)
    , Html.text (toString station.num_docks_available)
    ]

header : Html a
header = Html.thead [] [Html.tr [] [
                              Html.th [] [(Html.text "Station")]
                            , Html.th [] [(Html.text "Bikes")]
                            , Html.th [] [(Html.text "Vacancies")]
                            ]]

relevant_stations : List (StationStatus ID) -> List (StationStatus Name)
relevant_stations ss =
    let do_i_care stat =
            case Dict.get stat.station_id stations_i_care_about of
                Nothing   -> Nothing
                Just name -> Just { stat | station_id = name }
    in List.filterMap do_i_care ss

mk_tab stations =
    let rows = Html.tbody [] <| List.map (Html.tr [] << view_station) stations
    in Html.table [] [header, rows]
                  
        
view : State -> Html Msg
view model =
  Html.div []
    [
     case model of
         Loading  -> Html.text "Fetching Data..."
         Fail err -> Html.text (toString err)
         Stations stations ->
         let (home_stations, work_stations) = relevant_stations stations
             tables = List.map mk_tab [home_stations, work_stations]
         in div [] [home_tab, work_tab]
    ]
