{-# LANGUAGE OverloadedStrings #-}
import Java
import Network.Wai
import Network.HTTP.Types                 (status200)
import Network.Wai.Servlet
--import Network.Wai.Handler.Warp (run)
import Blaze.ByteString.Builder           (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
--import Control.Concurrent                 (threadDelay)
import Control.Concurrent.MVar
import Data.Monoid                        ((<>))
import System.IO.Unsafe


application :: Request -> (Response -> IO ResponseReceived) ->
               IO ResponseReceived
application _ respond = respond $
   responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

application' :: MVar Integer -> Request -> (Response -> IO ResponseReceived) ->
                IO ResponseReceived
application' countRef _ respond = do
    modifyMVar countRef $ \count -> do
        let count' = count + 1
            msg = fromByteString "You are visitor number: " <>
                  fromShow count'
        responseReceived <- respond $ responseBuilder
            status200
            [("Content-Type", "text/plain")]
            msg
        return (count', responseReceived)

foreign import java unsafe "@static Thread.sleep"
  threadDelay :: Int64 -> IO ()

application'' :: Request -> (Response -> IO ResponseReceived) ->
               IO ResponseReceived
application'' _ respond = respond $
  responseStream status200 [("Content-Type", "text/plain")]
    $ \send flush -> do
        send $ fromByteString "Starting the response...\n"
        flush
        threadDelay 1000000
        send $ fromByteString "All done!\n"

service :: DefaultWaiServletApplication
service = makeServiceMethod application

service' = makeServiceMethod $ app
  where app= application' $ unsafePerformIO $ newMVar 0

foreign export java "service" service' :: DefaultWaiServletApplication

main = undefined --run 3000 application

