{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             OverloadedStrings, TypeOperators #-}
module Network.Wai.Servlet.Request where
import Network.Wai.Internal
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr (SockAddrInet))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSChar (pack)
import Java

data {-# CLASS "javax.servlet.ServletRequest" #-}
  ServletRequest = ServletRequest (Object# ServletRequest)
  deriving Class

data {-# CLASS "javax.servlet.http.HttpServletRequest" #-}
  HttpServletRequest = HttpServletRequest (Object# HttpServletRequest)
  deriving Class

type instance Inherits HttpServletRequest = '[ServletRequest]

foreign import java unsafe "@interface getMethod" getMethod ::
  (a <: HttpServletRequest) => Java a String
foreign import java unsafe "@interface getVersion" getProtocol ::
  (a <: ServletRequest) => Java a String


makeWaiRequest :: HttpServletRequest -> Request
makeWaiRequest req  =  Request
   { requestMethod = getRequestMethod req 
   , httpVersion = getHttpVersion req
   , rawPathInfo = B.empty
   , rawQueryString = B.empty
   , requestHeaders = []
   , isSecure = False
   , remoteHost = SockAddrInet 0 0
   , pathInfo = []
   , queryString = []
   , requestBody = return B.empty
   , vault = mempty
   , requestBodyLength = KnownLength 0
   , requestHeaderHost = Nothing
   , requestHeaderRange = Nothing
   , requestHeaderReferer = Nothing
   , requestHeaderUserAgent = Nothing
  }

getRequestMethod :: (a <: HttpServletRequest) => a -> H.Method
getRequestMethod req = pureJavaWith req $ do
  method <- getMethod
  return $ BSChar.pack method

getHttpVersion ::  (a <: ServletRequest) => a -> H.HttpVersion
getHttpVersion req = pureJavaWith req $ do 
  httpVer <- getProtocol
  return $ case httpVer of
    "HTTP/0.9" -> H.http09
    "HTTP/1.0" -> H.http10
    "HTTP/1.1" -> H.http11
  
