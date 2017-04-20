{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             OverloadedStrings, TypeOperators #-}
module Network.Wai.Servlet.Request where
import qualified Network.Wai.Internal as W
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr (SockAddrInet,SockAddrInet6),
                       tupleToHostAddress,tupleToHostAddress6)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr_)
import Foreign.Ptr (Ptr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BSInt (fromForeignPtr)
import qualified Data.ByteString.Char8 as BSChar (pack)
import qualified Data.ByteString.UTF8 as BSUTF8 (fromString)
import qualified Data.CaseInsensitive as CI
import Data.Word
import Data.List (intercalate)
import Data.Maybe (fromMaybe,catMaybes)
import Java
import qualified Java.IO as JIO

data {-# CLASS "javax.servlet.ServletRequest" #-}
  ServletRequest = ServletRequest (Object# ServletRequest)
  deriving Class

data {-# CLASS "javax.servlet.http.HttpServletRequest" #-}
  HttpServletRequest = HttpServletRequest (Object# HttpServletRequest)
  deriving Class

data {-# CLASS "javax.servlet.ServletInputStream" #-}
  ServletInputStream = ServletInputStream (Object# ServletInputStream)
  deriving Class

type instance Inherits HttpServletRequest = '[ServletRequest]
type instance Inherits ServletInputStream = '[JIO.InputStream]

foreign import java unsafe "@interface getCharacterEncoding" getCharacterEncoding ::
  (a <: ServletRequest) => Java a String
foreign import java unsafe "@interface getProtocol" getProtocol ::
  (a <: ServletRequest) => Java a String
foreign import java unsafe "@interface isSecure" isSecure ::
  (a <: ServletRequest) => Java a Bool
foreign import java unsafe "@interface getRemoteAddr" getRemoteAddr ::
  (a <: ServletRequest) => Java a String
foreign import java unsafe "@interface getRemotePort" getRemotePort ::
  (a <: ServletRequest) => Java a Int
foreign import java unsafe "@interface getInputStream" getInputStream ::
  (a <: ServletRequest) => Java a ServletInputStream
foreign import java unsafe "@interface getContentLength" getContentLength ::
  (a <: ServletRequest) => Java a Int

foreign import java unsafe "@interface getMethod" getMethod ::
  (a <: HttpServletRequest) => Java a String
foreign import java unsafe "@interface getPathInfo" getPathInfo ::
  (a <: HttpServletRequest) => Java a (Maybe String)
foreign import java unsafe "@interface getQueryString" getQueryString ::
  (a <: HttpServletRequest) => Java a (Maybe String)
foreign import java unsafe "@interface getHeaderNames" getHeaderNames ::
  (a <: HttpServletRequest) => Java a (Enumeration JString)
foreign import java unsafe "@interface getHeaders" getHeaders ::
  (a <: HttpServletRequest) => String -> Java a (Maybe (Enumeration JString))

foreign import java unsafe "@static network.wai.servlet.Utils.toByteBuffer"
   toByteBuffer :: (is <: JIO.InputStream) => is -> Ptr Word8
foreign import java unsafe "@static network.wai.servlet.Utils.size"
   size :: Ptr Word8 -> Int

data SupportedCharEncoding = UTF8 | ISO_8859_1
  deriving Show

data RequestSettings = RequestSettings
  { reqSettingURICharEncoding :: SupportedCharEncoding }
  deriving Show

defaultRequestSettings :: RequestSettings
defaultRequestSettings = RequestSettings
  { reqSettingURICharEncoding = ISO_8859_1 }

makeWaiRequest :: HttpServletRequest -> W.Request
makeWaiRequest =  makeWaiRequestWithSettings defaultRequestSettings

makeWaiRequestWithSettings :: RequestSettings -> HttpServletRequest ->
                              W.Request
makeWaiRequestWithSettings settings req  =  W.Request
   { W.requestMethod = requestMethod req 
   , W.httpVersion = httpVersion req
   , W.rawPathInfo = rawPath
   , W.rawQueryString = rawQuery
   , W.requestHeaders = requestHeaders req
   , W.isSecure = isSecureRequest req
   , W.remoteHost = remoteHost req
   , W.pathInfo = path
   , W.queryString = query
   , W.requestBody = requestBody req
   , W.vault = mempty
   , W.requestBodyLength = requestBodyLength req
   , W.requestHeaderHost = header "Host"
   , W.requestHeaderRange = header "Range"
   , W.requestHeaderReferer = header "Referer"
   , W.requestHeaderUserAgent = header "User-Agent"
  }
  where uriCharEnc = reqSettingURICharEncoding settings
        rawPath = pathInfo uriCharEnc req
        path = H.decodePathSegments rawPath
        rawQuery = queryString uriCharEnc req
        query = H.parseQuery rawQuery
        header name = fmap snd $ requestHeader req name
          
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

encode ::  SupportedCharEncoding -> Maybe String -> B.ByteString
encode _ Nothing = B.empty
encode enc (Just str) = case enc of
  UTF8 -> BSUTF8.fromString str
  ISO_8859_1 -> BSChar.pack str
  
pathInfo :: (a <: HttpServletRequest) => SupportedCharEncoding ->
            a -> B.ByteString
pathInfo enc req = pureJavaWith req $ do
  path <- getPathInfo
  return $ encode enc path
                                  
queryString :: (a <: HttpServletRequest) => SupportedCharEncoding ->
               a -> B.ByteString
queryString enc req = pureJavaWith req $ do
  query <- getQueryString
  return $ encode enc query
  
requestHeaders :: (a <: HttpServletRequest) => a -> H.RequestHeaders
requestHeaders req = pureJavaWith req $ do
  names <- getHeaderNames
  return $ catMaybes $ map (requestHeader req . fromJString) $
           fromJava  names
  
requestHeader ::  (a <: HttpServletRequest) => a -> String -> Maybe H.Header
requestHeader req name = pureJavaWith req $ do
  mjhdrs <- getHeaders name
  return $ fmap f mjhdrs
  where f jhdrs = (hdrn,hdrs')
          where hdrs = map fromJString $ fromJava jhdrs
                hdrs' = BSChar.pack $ intercalate "," hdrs
                hdrn = CI.mk $ BSChar.pack name 

isSecureRequest :: (a <: ServletRequest) => a -> Bool
isSecureRequest req = pureJavaWith req $ isSecure

remoteHost :: (a <: ServletRequest) => a -> SockAddr
remoteHost req = pureJavaWith req $ do
  ipStr <- getRemoteAddr
  portInt <- getRemotePort
  let ip = wordsWhen (=='.') ipStr
      ip' = if (length ip == 1) then wordsWhen (==':') ipStr else ip
      port = fromIntegral portInt
  return $ case length ip' of
    4 -> let [ip1,ip2,ip3,ip4] = map read ip' in 
         SockAddrInet port $ tupleToHostAddress (ip1,ip2,ip3,ip4)
    8 -> let [ip1,ip2,ip3,ip4,ip5,ip6,ip7,ip8] = map read ip' in
         SockAddrInet6 port 0
         (tupleToHostAddress6 (ip1,ip2,ip3,ip4,ip5,ip6,ip7,ip8)) 0
    _ -> error $ "Error parsing ip: " ++ ipStr 

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

requestBody :: (a <: ServletRequest) => a -> IO B.ByteString
requestBody req = do
  is <- javaWith req getInputStream
  let ptr = toByteBuffer is
      l = size ptr
  fptr <- newForeignPtr_ ptr -- without finalizer?
  return $ if l == 0 then B.empty
           else BSInt.fromForeignPtr fptr 0 l 
  
requestBodyLength :: (a <: ServletRequest) => a -> W.RequestBodyLength
requestBodyLength req = pureJavaWith req $ do
  l <- getContentLength
  return $ if l > -1 then W.KnownLength (fromIntegral l)
           else W.ChunkedBody
