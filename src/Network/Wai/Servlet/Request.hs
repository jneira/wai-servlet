{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeOperators,
             ScopedTypeVariables, CPP #-}
module Network.Wai.Servlet.Request
    ( HttpServletRequest
    , ServletRequest
    , makeWaiRequest
    , makeWaiRequestSettings
    , requestHeaders
    , requestMethod ) where
import qualified Network.Wai.Internal as W
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr (SockAddrInet,SockAddrInet6),
                       tupleToHostAddress,tupleToHostAddress6)
import Foreign.ForeignPtr (ForeignPtr,newForeignPtr,newForeignPtr_)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Ptr (Ptr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BSInt (fromForeignPtr)
import qualified Data.ByteString.Char8 as BSChar (pack)
import qualified Data.ByteString.UTF8 as BSUTF8 (fromString)
import qualified Data.CaseInsensitive as CI
import Data.Word
import Data.List (intercalate)
import Data.Maybe (fromMaybe,catMaybes)
import Control.Monad (when)
import Java
import Java.Array
#ifdef INTEROP
import qualified Interop.Java.IO as JIO
#else
import qualified Java.IO as JIO
#endif
import Javax.Servlet
import Network.Wai.Servlet.Settings

doJavaWith :: (Class c) => c -> Java c a -> a
#ifdef PURE_JAVA_WITH
doJavaWith = pureJavaWith
#else
doJavaWith = unsafePerformJavaWith
#endif

foreign import java unsafe "@static network.wai.servlet.Utils.toByteArray"
   toByteArray :: (is <: JIO.InputStream) => is -> JByteArray
foreign import java unsafe "@static network.wai.servlet.Utils.toByteBuffer"
   toByteBuffer :: JByteArray -> Ptr Word8
foreign import java unsafe "@static network.wai.servlet.Utils.size"
   size :: Ptr Word8 -> Int

makeWaiRequest :: HttpServletRequest -> W.Request
makeWaiRequest = makeWaiRequestSettings defaultSettings  

makeWaiRequestSettings :: Settings -> HttpServletRequest
                       -> W.Request
makeWaiRequestSettings settings req = W.Request
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
   , W.requestHeaderUserAgent = header "User-Agent" }
  where encoding = getUriEncoding settings
        rawPath = rawPathInfo encoding req
        path = H.decodePathSegments $ pathInfo encoding req
        rawQuery = queryString encoding req
        query = H.parseQuery rawQuery
        header name = fmap snd $ requestHeader req name

requestMethod :: (a <: HttpServletRequest) => a -> H.Method
requestMethod req = doJavaWith req $ do
  method <- getMethod
  return $ BSChar.pack method

httpVersion ::  (a <: ServletRequest) => a -> H.HttpVersion
httpVersion req = doJavaWith req $ do
  httpVer <- getProtocol
  return $ case httpVer of
    "HTTP/0.9" -> H.http09
    "HTTP/1.0" -> H.http10
    "HTTP/1.1" -> H.http11
    "HTTP/2.0" -> H.http20

encode ::  CharEncoding -> Maybe String -> B.ByteString
encode _ Nothing = B.empty
encode UTF8 (Just str) =  BSUTF8.fromString str
encode ISO88591 (Just str) = BSChar.pack str

rawPathInfo :: (a <: HttpServletRequest) => CharEncoding -> a -> B.ByteString
rawPathInfo enc req = doJavaWith req $ do
  path <- getPathInfo
  case path of
    Nothing -> return B.empty
    Just str -> do
      let segments = wordsWhen (=='/') str
      return $ B.intercalate "/" $
        map (H.urlEncode False . encode enc . Just) segments

pathInfo :: (a <: HttpServletRequest) => CharEncoding -> a -> B.ByteString
pathInfo enc req = doJavaWith req $ do
  path <- getPathInfo
  return $ encode enc path

queryString :: (a <: HttpServletRequest) => CharEncoding -> a -> B.ByteString
queryString enc req = doJavaWith req $ do
  query <- getQueryString
  return $ encode enc query

requestHeaders :: (a <: HttpServletRequest) => a -> H.RequestHeaders
requestHeaders req = doJavaWith req $ do
  names <- getHeaderNames
  return $ catMaybes $ map (requestHeader req . fromJString) $
           fromJava  names

requestHeader ::  (a <: HttpServletRequest) => a -> String -> Maybe H.Header
requestHeader req name = doJavaWith req $ do
  mjhdrs <- getHeaders name
  return $ mjhdrs >>= f
  where f jhdrs = if null hdrs then Nothing else Just (hdrn,hdrs')
          where hdrs = map fromJString $ fromJava jhdrs
                hdrs' = BSChar.pack $ intercalate "," hdrs
                hdrn = CI.mk $ BSChar.pack name 

isSecureRequest :: (a <: ServletRequest) => a -> Bool
isSecureRequest req = doJavaWith req $ isSecure

remoteHost :: (a <: ServletRequest) => a -> SockAddr
remoteHost req = doJavaWith req $ do
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
  let bytes = toByteArray is
      ptr   = toByteBuffer bytes
  l    <- javaWith bytes alength
  fptr <- newForeignPtr_ ptr
  -- fptr <- newForeignPtr finalizerFree ptr
  -- TODO: There is a bug in the MemoryManager that prevents us from using this.
  return $ if l == 0 then B.empty
           else BSInt.fromForeignPtr fptr 0 l

requestBodyLength :: (a <: ServletRequest) => a -> W.RequestBodyLength
requestBodyLength req = doJavaWith req $ do
  l <- getContentLength
  return $ W.KnownLength (fromIntegral $ if l < 0 then 0 else l)
