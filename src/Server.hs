{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Client (fetchDepartures)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Proxy
import Data.Text
import Servant
import Servant.API
import Types

type DepartureApi = "departures" :> Get '[JSON] DepartureBoard

server :: Server DepartureApi
server = do
  result <- liftIO fetchDepartures
  case result of
    Left _ -> throwError fetchError
    Right departureBoard -> return departureBoard
  where
    fetchError = err503 {errBody = "Failed to fetch departureBoard from Rejseplanen."}

departureApi :: Proxy DepartureApi
departureApi = Proxy

app :: Application
app = serve departureApi server
