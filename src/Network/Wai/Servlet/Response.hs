{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,OverloadedStrings #-}
module Network.Wai.Servlet.Response where
import Control.Monad (forM_)
import qualified Data.CaseInsensitive as CI (original)
import qualified Data.ByteString.Char8 as BSChar (unpack)  
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiIn
import qualified Network.HTTP.Types as HTTP
import Java

data {-# CLASS "javax.servlet.ServletResponse" #-} ServletResponse =
  ServletResponse (Object# ServletResponse) deriving Class

data {-# CLASS "javax.servlet.HttpServletResponse" #-} HttpServletResponse =
  HttpServletResponse (Object# HttpServletResponse) deriving Class

foreign import java unsafe setStatus ::  Int -> Java HttpServletResponse ()
foreign import java unsafe setHeader ::  String -> String ->
                                         Java HttpServletResponse ()

type instance Inherits HttpServletResponse = '[ServletResponse]

setStatusAndHeaders :: HTTP.Status -> [HTTP.Header] ->
                       Java HttpServletResponse ()  
setStatusAndHeaders status headers = do
  setStatus $ HTTP.statusCode status
  forM_ headers $ \ (name,value) -> do
    setHeader (BSChar.unpack $ CI.original  name)
              (BSChar.unpack value)
  
updateHttpServletResponse :: HttpServletResponse -> Wai.Response ->
                         IO Wai.ResponseReceived
updateHttpServletResponse servResp waiResp = case waiResp of
  (WaiIn.ResponseFile status headers filePath filePart) -> undefined
  (WaiIn.ResponseBuilder status headers builder) -> do
    javaWith servResp $ setStatusAndHeaders status headers
    return WaiIn.ResponseReceived
  (WaiIn.ResponseStream status headers body) -> undefined
  (WaiIn.ResponseRaw rawStream response) -> undefined

