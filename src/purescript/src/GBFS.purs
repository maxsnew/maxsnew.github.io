module GBFS where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Eq
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.StrMap as SM

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Arg
-- | API for General Bikeshare Feed Specification (GBFS)
-- | TODO: make these extensible records

type StationId = String

type GbfsData d = { last_updated :: Number
                  , ttl :: Number
                  , data' :: d
                  }

-- mapGbfsData :: forall a b. (a -> b) -> GbfsData a -> GbfsData b
-- mapGbfsData f d = 

type StationStatus = { station_id :: StationId
                     , num_bikes_available :: Number
                     -- , num_bikes_disabled :: Maybe Number
                     , num_docks_available :: Number
                     -- , num_docks_disabled :: Maybe Number
                     , is_renting :: Boolean
                     , is_installed :: Boolean
                     , is_returning :: Boolean
                     , last_reported :: Number
                     }

type StationInformation = { station_id :: StationId
                          , name :: String
                          , short_name :: String
                          , lat :: Number
                          , lon :: Number
                          -- TODO: some optional fields
                          -- , address :: Maybe String
                          }

parseGbfs :: Arg.Json -> Either String (GbfsData Json)
parseGbfs =
  Arg.foldJsonObject
    expectedObject
    (\smap ->
      { last_updated: _, ttl: _, data': _ } <$> (parseNumber =<< lookupExpect "last_updated" smap)
                                            <*> (parseNumber =<< lookupExpect "ttl" smap)
                                            <*> lookupExpect "data" smap
    )
  where
    expectedObject = Left "Expected GBFS data object"

lookupExpect :: forall a. String -> SM.StrMap a -> Either String a
lookupExpect fld = lookupField ("Expected object to contain " <> fld <> " field") fld
lookupField :: forall e a. e -> String -> SM.StrMap a -> Either e a
lookupField err fld smap = maybe (Left err) Right $ SM.lookup fld smap


parseStationStatuses :: Arg.Json -> Either String (GbfsData (Array StationStatus))
parseStationStatuses js = do
  gbfs <- parseGbfs js
  let d = gbfs.data'
  rawStations <- Arg.foldJsonObject expectedObject (lookupExpect "stations") d
  statuses <- Arg.foldJsonArray expectedArray (traverse parseStationStatus) rawStations
  pure $ gbfs { data' = statuses }
  where
    expectedArray = Left "Expected Array of StationStatuses"
    expectedObject = Left "Expected Object with staitons field of StationStatuses"

parseStationStatus :: Arg.Json -> Either String StationStatus
parseStationStatus =
  Arg.foldJsonObject
    (Left "Expected StationStatus")
    (\smap ->
      { station_id: _
      , num_bikes_available: _
      -- , num_bikes_disabled: _
      , num_docks_available: _
      -- , num_docks_disabled: _
      , is_renting: _
      , is_installed: _
      , is_returning: _
      , last_reported: _
      } <$> (parseStr =<< lookupExpect "station_id" smap)
        <*> (parseNumber =<< lookupExpect "num_bikes_available" smap)
        -- <*> (parseNumber =<< lookupExpect "num_bikes_disable" smap)
        <*> (parseNumber =<< lookupExpect "num_docks_available" smap)
        -- <*> (parseNumber =<< lookupExpect "num_bikes_disabled" smap)
        <*> (parseBool =<< lookupExpect "is_renting" smap)
        <*> (parseBool =<< lookupExpect "is_installed" smap)
        <*> (parseBool =<< lookupExpect "is_returning" smap)
        <*> (parseNumber =<< lookupExpect "last_reported" smap)
      )

parseStr :: Arg.Json -> Either String String
parseStr = Arg.foldJsonString (expected "string") pure

-- Unsafe: should check if it's really an int
parseNumber :: Arg.Json -> Either String Number
parseNumber = Arg.foldJsonNumber (expected "number") pure

parseBool :: Arg.Json -> Either String Boolean
parseBool = Arg.foldJsonNumber (expected "boolean") (pure <<< numToBool)
  where numToBool n = (n /= 0.0)

parseError :: forall a. Either String a
parseError = Left "ParseError (sorry for the shitty error)"

expected :: forall a. String -> Either String a
expected s = Left ("")
