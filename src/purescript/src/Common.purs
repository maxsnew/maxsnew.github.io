module Common where

import Prelude

import GBFS as GBFS

type ResolvedStation = {info :: GBFS.StationInformation, status :: GBFS.StationStatus }

type Coordinates r = { lat :: Number, lon :: Number | r }
type Place = Coordinates (name :: String)

-- | Constants

statusUrl :: String
statusUrl = "https://gbfs.thehubway.com/gbfs/en/station_status.json"

infoUrl :: String
infoUrl = "https://gbfs.thehubway.com/gbfs/en/station_information.json"

home :: Place
home = { name : "home", lat : 42.34852014581272, lon : -71.13394662737846 }

work :: Place
work = { name : "work", lat : 42.33863, lon : -71.092228 }
