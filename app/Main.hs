module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Server (app)

main :: IO ()
main = do
  print "Running relay server on port 8081"
  run 8081 app
