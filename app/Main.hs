{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

application :: Request -> (Response -> IO ResponseReceived) ->
               IO ResponseReceived
application _ respond = respond $
  responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main = run 3000 application
