{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text
import Data.Time
import GHC.Generics

data TransportMethod
  = InterCityTrain
  | SpeedTrain
  | RegionalTrain
  | STrain
  | Train
  | Bus
  | ExpressBus
  | NightBus
  | TrainBus
  | Ferry
  | Metro
  deriving (Show, Generic, ToJSON)

instance FromJSON TransportMethod where
  parseJSON (String v) = pure $
    case v of
      "IC" -> InterCityTrain
      "LYN" -> SpeedTrain
      "REG" -> RegionalTrain
      "S" -> STrain
      "TOG" -> Train
      "BUS" -> Bus
      "EXB" -> ExpressBus
      "NB" -> NightBus
      "TB" -> TrainBus
      "F" -> Ferry
      "M" -> Metro

data Departure
  = Departure
      { name :: Text,
        method :: TransportMethod,
        stop :: Text,
        datetime :: UTCTime
      }
  deriving (Show, Generic, ToJSON)

instance FromJSON Departure where
  parseJSON (Object v) =
    Departure
      <$> v .: "name"
      <*> v .: "type"
      <*> v .: "stop"
      <*> datetime
    where
      datetimeParser = parseTimeM False defaultTimeLocale "%d.%m.%y%H:%M"
      datetime = do
        date <- v .: "date"
        time <- v .: "time"
        datetimeParser (date ++ time)

newtype DepartureBoard
  = DepartureBoard
      { departures :: [Departure]
      }
  deriving (Show, Generic, ToJSON)

instance FromJSON DepartureBoard where
  parseJSON (Object v) = do
    departureBoard <- v .: "DepartureBoard"
    DepartureBoard <$> departureBoard .: "Departure"
