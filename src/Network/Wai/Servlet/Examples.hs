{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Servlet.Examples where
import Java
import Network.Wai
import Network.HTTP.Types                 (status200)
import Network.Wai.Servlet
--import Network.Wai.Handler.Warp (run)
import Blaze.ByteString.Builder           (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow,fromString)
--import Control.Concurrent                 (threadDelay)
import Control.Concurrent.MVar
import Data.Monoid                        ((<>))
import System.IO.Unsafe


appSimple :: Application
appSimple _ respond = respond $
   responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

appState :: MVar Integer -> Application
appState countRef _ respond = do
    modifyMVar countRef $ \count -> do
        let count' = count + 1
            msg = fromByteString "You are visitor number: " <>
                  fromShow count'
        responseReceived <- respond $ responseBuilder
            status200
            [("Content-Type", "text/plain")]
            msg
        return (count', responseReceived)

foreign import java unsafe "@static java.lang.Thread.sleep"
  threadDelay :: Int64 -> IO ()

appStream :: Application
appStream _ respond = respond $
  responseStream status200 [("Content-Type", "text/plain")]
    $ \send flush -> do
        send $ fromByteString "Starting the response...\n"
        flush
        threadDelay 30000
        send $ fromByteString "All done!\n"

servSimple :: DefaultWaiServletApplication
servSimple = makeServiceMethod appSimple

servState :: DefaultWaiServletApplication
servState = makeServiceMethod $ app
  where app = appState $ unsafePerformIO $ newMVar 0

servStream :: DefaultWaiServletApplication
servStream = makeServiceMethod appStream

appFile :: FilePath -> Application
appFile path _ respond = respond $
  responseFile status200 [("Content-Type", "text/html")]
    path Nothing

servFile :: DefaultWaiServletApplication
servFile = makeServiceMethod $ appFile "index.html"

appShowReq :: Application
appShowReq req respond = do
  body <- requestBody req
  resp <- respond $ responseBuilder status200
                    [("Content-Type", "text/plain")] $
                    fromShow req <> fromString "\n" <>
                    fromByteString body
  return resp

servShowReq :: DefaultWaiServletApplication
servShowReq = makeServiceMethod appShowReq 

foreign export java "service" servShowReq :: DefaultWaiServletApplication



