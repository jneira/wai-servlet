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

application2 :: MVar Integer -> Request -> (Response -> IO ResponseReceived) ->
               IO ResponseReceived
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

application3 :: Request -> (Response -> IO ResponseReceived) ->
               IO ResponseReceived
application3 _ respond = respond $
  responseStream status200 [("Content-Type", "text/plain")]
    $ \send flush -> do
        send $ fromByteString "Starting the response...\n"
        flush
        threadDelay 1000000
        send $ fromByteString "All done!\n"

service :: DefaultWaiServletApplication
service = makeServiceMethod application

service' :: DefaultWaiServletApplication
service' = makeServiceMethod application2'
  where application2' req respond = do
          cnt <- newMVar 0
          application2 cnt req respond

foreign export java "service" service' :: DefaultWaiServletApplication

main = undefined --run 3000 application
