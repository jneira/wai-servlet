{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             OverloadedStrings, TypeOperators #-}
module Network.Wai.Servlet.Request where
import qualified Network.Wai.Internal as W
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr (SockAddrInet))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSChar (pack)
import qualified Data.ByteString.UTF8 as BSUTF8 (toString)
import Data.List (intercalate)
import Java

data {-# CLASS "javax.servlet.ServletRequest" #-}
  ServletRequest = ServletRequest (Object# ServletRequest)
  deriving Class

data {-# CLASS "javax.servlet.http.HttpServletRequest" #-}
  HttpServletRequest = HttpServletRequest (Object# HttpServletRequest)
  deriving Class

type instance Inherits HttpServletRequest = '[ServletRequest]

foreign import java unsafe "@interface getCharacterEncoding" getCharacterEncoding ::
  (a <: ServletRequest) => Java a String
foreign import java unsafe "@interface getVersion" getProtocol ::
  (a <: ServletRequest) => Java a String
foreign import java unsafe "@interface getMethod" getMethod ::
  (a <: HttpServletRequest) => Java a String
foreign import java unsafe "@interface getPathInfo" getPathInfo ::
  (a <: HttpServletRequest) => Java a String
foreign import java unsafe "@interface getHeaderNames" getHeaderNames ::
  (a <: HttpServletRequest) => Java a (Enumeration String)
foreign import java unsafe "@interface getHeaders" getHeaders ::
  (a <: HttpServletRequest) => String -> Java a (Enumeration String)


makeWaiRequest :: HttpServletRequest -> W.Request
makeWaiRequest req  =  W.Request
   { W.requestMethod = requestMethod req 
   , W.httpVersion = httpVersion req
   , W.rawPathInfo = pathInfo req
   , W.rawQueryString = B.empty
   , W.requestHeaders = []
   , W.isSecure = False
   , W.remoteHost = SockAddrInet 0 0
   , W.pathInfo = H.extractPath $ pathInfo req
   , W.queryString = []
   , W.requestBody = return B.empty
   , W.vault = mempty
   , W.requestBodyLength = W.KnownLength 0
   , W.requestHeaderHost = Nothing
   , W.requestHeaderRange = Nothing
   , W.requestHeaderReferer = Nothing
   , W.requestHeaderUserAgent = Nothing
  }

requestMethod :: (a <: HttpServletRequest) => a -> H.Method
requestMethod req = pureJavaWith req $ do
  method <- getMethod
  return $ BSChar.pack method

httpVersion ::  (a <: ServletRequest) => a -> H.HttpVersion
httpVersion req = pureJavaWith req $ do 
  httpVer <- getProtocol
  return $ case httpVer of
    "HTTP/0.9" -> H.http09
    "HTTP/1.0" -> H.http10
    "HTTP/1.1" -> H.http11
  
pathInfo :: (a <: ServletRequest) => a -> B.ByteString
pathInfo req = pureJavaWith req $ do
  path <- getPathInfo
  return $ BSUTF8.fromString path

requestHeaders :: (a <: ServletRequest) => a -> H.RequestHeaders
requestHeaders req = pureJavaWith req $ do
  names <- fromJava getHeaderNames
  return $  zip names $ map (BSUTF8.fromString . intercalate "," .
                             fromJava . getHeaders) names
  
  
