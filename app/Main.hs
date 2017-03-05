{-# LANGUAGE OverloadedStrings #-}
import Java
import Network.Wai
import Network.HTTP.Types                 (status200)
import Network.Wai.Servlet
--import Network.Wai.Handler.Warp (run)
import Blaze.ByteString.Builder           (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import Control.Concurrent                 (threadDelay)
import Control.Concurrent.MVar
import Data.Monoid                        ((<>))



application :: Request -> (Response -> IO ResponseReceived) ->
               IO ResponseReceived
application _ respond = respond $
   responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

application2 countRef _ respond = do
    modifyMVar countRef $ \count -> do
        let count' = count + 1
            msg = fromByteString "You are visitor number: " <>
                  fromShow count'
        responseReceived <- respond $ responseBuilder
            status200
            [("Content-Type", "text/plain")]
            msg
        return (count', responseReceived)

application3 _ respond = respond $
  responseStream status200 [("Content-Type", "text/plain")]
    $ \send flush -> do
        send $ fromByteString "Starting the response...\n"
        flush
        threadDelay 1000000
        send $ fromByteString "All done!\n"

service :: WaiServletApplication
service = makeServiceMethod application

foreign export java service :: WaiServletApplication

main = undefined --run 3000 application
