module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.Cors (simpleCors)
import Server (app)

main :: IO ()
main = do
  print "Running API server on port 8081"
  runTLS tlsOptions warpOptions app'
  where
    app' = simpleCors app
    tlsOptions = tlsSettings "cert.pem" "secret-key.pem"
    warpOptions = setPort 8081 defaultSettings
