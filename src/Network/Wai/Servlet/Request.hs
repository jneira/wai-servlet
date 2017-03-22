{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             OverloadedStrings, TypeOperators #-}
module Network.Wai.Servlet.Request where
import qualified Network.Wai.Internal as W
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr (SockAddrInet))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BSChar (pack)
import qualified Data.ByteString.UTF8 as BSUTF8 (fromString)
import qualified Data.CaseInsensitive as CI
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
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
foreign import java unsafe "@interface getQueryString" getQueryString ::
  (a <: HttpServletRequest) => Java a (Maybe String)
foreign import java unsafe "@interface getHeaderNames" getHeaderNames ::
  (a <: HttpServletRequest) => Java a (Enumeration JString)
foreign import java unsafe "@interface getHeaders" getHeaders ::
  (a <: HttpServletRequest) => String -> Java a (Enumeration JString)


makeWaiRequest :: HttpServletRequest -> W.Request
makeWaiRequest req  =  W.Request
   { W.requestMethod = requestMethod req 
   , W.httpVersion = httpVersion req
   , W.rawPathInfo = rawPath
   , W.rawQueryString = rawQuery
   , W.requestHeaders = []
   , W.isSecure = False
   , W.remoteHost = SockAddrInet 0 0
   , W.pathInfo = path
   , W.queryString = query
   , W.requestBody = return B.empty
   , W.vault = mempty
   , W.requestBodyLength = W.KnownLength 0
   , W.requestHeaderHost = Nothing
   , W.requestHeaderRange = Nothing
   , W.requestHeaderReferer = Nothing
   , W.requestHeaderUserAgent = Nothing
  }
  where rawPath = pathInfo req
        path = H.decodePathSegments rawPath
        rawQuery = queryString req
        query = H.parseQuery rawQuery
          
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
  
pathInfo :: (a <: HttpServletRequest) => a -> B.ByteString
pathInfo req = pureJavaWith req $ do
  path <- getPathInfo
  return $ BSUTF8.fromString path

queryString :: (a <: HttpServletRequest) => a -> B.ByteString
queryString req = pureJavaWith req $ do
  query <- getQueryString
  let queryStr = fromMaybe "" query
  return $ BSUTF8.fromString queryStr

requestHeaders :: (a <: HttpServletRequest) => a -> H.RequestHeaders
requestHeaders req = pureJavaWith req $ do
  names <- getHeaderNames
  return $ map (requestHeader req . fromJString) $ fromJava  names
  
requestHeader ::  (a <: HttpServletRequest) => a -> String -> H.Header
requestHeader req name = pureJavaWith req $ do
  jhdrs <- getHeaders name
  let hdrs = map fromJString $ fromJava jhdrs
      hdrs' = BSChar.pack $ intercalate "," hdrs
      hdrn = CI.mk $ BSChar.pack name 
  return (hdrn,hdrs')



