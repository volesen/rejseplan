{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Data.Text
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.Client
import Types

type DepartureBoardAPI =
  "departureBoard"
    :> QueryParam "id" Int
    :> QueryParam "format" Text
    :> Get '[JSON] DepartureBoard

departureBoardAPI :: Proxy DepartureBoardAPI
departureBoardAPI = Proxy

departureApiClient' :: Maybe Int -> Maybe Text -> ClientM DepartureBoard
departureApiClient' = client departureBoardAPI

departureApiClient :: Int -> Text -> ClientM DepartureBoard
departureApiClient id format = departureApiClient' (Just id) (Just format)

fetchDepartures :: IO (Either ClientError DepartureBoard)
fetchDepartures = do
  manager' <- newManager defaultManagerSettings
  baseUrl <- parseBaseUrl "http://rejseplanen.dk/bin/rest.exe/"
  liftIO $ runClientM (departureApiClient 8600690 "json") (mkClientEnv manager' baseUrl)
