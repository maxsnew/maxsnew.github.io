module Main exposing (..)

import Dict
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode exposing (field, at, Decoder)
import Task exposing (andThen, succeed)


debug =
    False



-- | Displays the current bike capacity for the hubway stations I care about
-- | Constants


type alias ID =
    String


type alias Name =
    String


type alias Coordinates =
    { longitude : Float
    , latitude : Float
    }


type alias Place =
    { name : Name, coord : Coordinates }


status_url =
    "https://gbfs.thehubway.com/gbfs/en/station_status.json"


info_url =
    "https://gbfs.thehubway.com/gbfs/en/station_information.json"



-- https://gbfs.thehubway.com/gbfs/en/station_information.json
-- could do this more automatically


home : Place
home =
    { name = "home"
    , coord =
        { latitude = 42.34852014581272
        , longitude = -71.13394662737846
        }
    }


work : Place
work =
    { name = "work"
    , coord =
        { latitude = 42.33863
        , longitude = -71.092228
        }
    }


main =
    Html.program { init = init, view = view, update = update, subscriptions = \_ -> Sub.none }



-- MODEL


type alias StationInfo =
    { id : ID
    , name : Name
    , coord : Coordinates
    }


type alias StationStatus name =
    { station_id : name
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


type alias StationData =
    { id : ID
    , name : Name
    , coord : Coordinates
    , num_bikes_available : Int
    , num_docks_available : Int
    }


type alias HubwayData =
    List StationData


type State
    = Loading
    | Fail Http.Error
    | Loaded (List ( Place, HubwayData ))


mk_station_status : name -> Int -> Int -> StationStatus name
mk_station_status id bikes docks =
    { station_id = id
    , num_bikes_available = bikes
    , num_docks_available = docks
    }


station_status : Decoder (StationStatus ID)
station_status =
    Decode.map3 mk_station_status
        (field "station_id" Decode.string)
        (field "num_bikes_available" Decode.int)
        (field "num_docks_available" Decode.int)


status_decoder : Decoder (List (StationStatus ID))
status_decoder =
    at [ "data", "stations" ] (Decode.list station_status)


mk_station_info id name long lat =
    { id = id
    , name = name
    , coord =
        { longitude = long
        , latitude = lat
        }
    }


station_info : Decoder StationInfo
station_info =
    Decode.map4 mk_station_info
        (field "station_id" Decode.string)
        (field "name" Decode.string)
        (field "lon" Decode.float)
        (field "lat" Decode.float)


info_decoder : Decoder (List StationInfo)
info_decoder =
    at [ "data", "stations" ] (Decode.list station_info)


getTask s dec =
    Http.get s dec |> Http.toTask


fetch : Cmd Msg
fetch =
    Task.attempt identity
        (getTask status_url status_decoder
            |> andThen
                (\statuses ->
                    getTask info_url info_decoder
                        |> andThen
                            (\infos ->
                                succeed ( infos, statuses )
                            )
                )
        )



-- Http.send identity <|
--     getTask "https://gbfs.thehubway.com/gbfs/en/station_status.json" status_decoder


init : ( State, Cmd Msg )
init =
    ( Loading, fetch )



-- UPDATE


type alias Msg =
    Result Http.Error ( List StationInfo, List (StationStatus ID) )


update : Msg -> State -> ( State, Cmd Msg )
update new_info st =
    case new_info of
        Err e ->
            ( Fail e, Cmd.none )

        Ok ( infos, statuses ) ->
            let
                hwData =
                    mergeHubwayData infos statuses

                places =
                    [ home, work ]

                nearbys =
                    List.map (nearby hwData) places
            in
                ( Loaded nearbys, Cmd.none )


mergeStationData : StationInfo -> StationStatus ID -> StationData
mergeStationData inf stat =
    { id = inf.id
    , name = inf.name
    , coord = inf.coord
    , num_bikes_available = stat.num_bikes_available
    , num_docks_available = stat.num_docks_available
    }


mergeHubwayData : List StationInfo -> List (StationStatus ID) -> HubwayData
mergeHubwayData infos statuses =
    let
        infoDict =
            List.foldl (\info d -> Dict.insert info.id info d) Dict.empty infos

        hwDict =
            List.foldl
                (\stat l ->
                    case Dict.get stat.station_id infoDict of
                        Just inf ->
                            mergeStationData inf stat :: l

                        Nothing ->
                            l
                )
                []
                statuses
    in
        hwDict



-- | Square of the distance


proximity : Coordinates -> Coordinates -> Float
proximity p1 p2 =
    (p1.latitude - p2.latitude) ^ 2 + (p1.longitude - p2.longitude) ^ 2


nearby : HubwayData -> Place -> ( Place, HubwayData )
nearby h p =
    let
        proxToP stat =
            proximity p.coord stat.coord

        sorted =
            List.sortBy proxToP h

        closes =
            List.filter (\s -> proxToP s <= cutoff) sorted
    in
        ( p, closes )


cutoff =
    0.000175



-- VIEW


view_station : StationData -> List (Html a)
view_station station =
    List.map (Html.td [] << (\x -> [ x ]))
        [ Html.text (station.name)
        , Html.text (toString station.num_bikes_available)
        , Html.text (toString station.num_docks_available)
        ]


header : Html a
header =
    Html.thead []
        [ Html.tr []
            [ Html.th [] [ (Html.text "Station") ]
            , Html.th [] [ (Html.text "Bikes") ]
            , Html.th [] [ (Html.text "Vacancies") ]
            ]
        ]


mk_tab : List StationData -> Html a
mk_tab stations =
    let
        rows =
            Html.tbody [] <| List.map (Html.tr [] << view_station) stations
    in
        Html.table [] [ header, rows ]


view_near_place : Place -> HubwayData -> Html a
view_near_place p hw =
    Html.div []
        [ Html.h2 [] [ Html.text ("Places near " ++ p.name) ]
        , mk_tab hw
        ]


view : State -> Html Msg
view model =
    Html.div []
        [ case model of
            Loading ->
                Html.text "Fetching Data..."

            Fail err ->
                Html.text (toString err)

            Loaded nearbys ->
                Html.div []
                    ((List.map (\( x, y ) -> view_near_place x y) nearbys)
                        ++ debugInfo nearbys
                    )
        ]


debugInfo : a -> List (Html b)
debugInfo x =
    if debug then
        [ Html.text (toString x) ]
    else
        []
