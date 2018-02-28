{-# LANGUAGE FlexibleContexts, TypeOperators, RecordWildCards,
             OverloadedStrings, CPP, ScopedTypeVariables #-}
module Network.Wai.Servlet.Response
    ( HttpServletResponse
    , ServletResponse
    , updateHttpServletResponse ) where
import Control.Monad (forM_,when,unless)
import Control.Exception as E
import Data.Function (on)
import Data.List (deleteBy)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.CaseInsensitive as CI (original)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSLInt
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSInt
import qualified Data.ByteString.Char8 as BSChar (unpack)
import Foreign.ForeignPtr (ForeignPtr,withForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiIn
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Network.Wai.Servlet.Request
import Network.Wai.Servlet.File
import Network.Wai.Servlet.Settings
import Java
#ifdef INTEROP
import qualified Interop.Java.IO as JIO
#else
import qualified Java.IO as JIO
#endif
import Java.Exception
import Javax.Servlet

foreign import java unsafe "@static network.wai.servlet.Utils.toByteArray"
   toByteArray :: Ptr Word8 -> Int -> Int -> JByteArray

updateHttpServletResponse :: HttpServletRequest -> HttpServletResponse
                          -> Wai.Response -> IO Wai.ResponseReceived
updateHttpServletResponse = updateHttpServletResponseSettings defaultSettings

updateHttpServletResponseSettings :: Settings
                                  -> HttpServletRequest -> HttpServletResponse
                                  -> Wai.Response -> IO Wai.ResponseReceived
updateHttpServletResponseSettings settings servReq servResp waiResp =
  javaWith servResp $ do
  case waiResp of
    (WaiIn.ResponseBuilder status headers builder) ->
      sendRspBuilder status headers builder

    (WaiIn.ResponseStream status headers body) -> 
      io $ sendRspStream servResp status headers body 

    (WaiIn.ResponseFile status headers filePath filePart) -> do
      let isHead = requestMethod servReq == HTTP.methodHead 
      sendRspFile settings status headers (requestHeaders servReq)
                  filePath filePart isHead
                                             
    (WaiIn.ResponseRaw rawStream response) ->
      error "ResponseRaw not supported by wai-servlet"
  return WaiIn.ResponseReceived

sendRspBuilder :: HTTP.Status -> [HTTP.Header] -> Blaze.Builder
               -> Java HttpServletResponse ()  
sendRspBuilder status headers builder = do
  setStatusAndHeaders status headers
  buffSize <- getBufferSize
  when (hasBody status) $ 
    writeLazyByteString $ toLazyByteString buffSize builder

sendRspStream :: HttpServletResponse -> HTTP.Status
              -> [HTTP.Header] -> WaiIn.StreamingBody -> IO ()  
sendRspStream servResp status headers body = do
  javaWith servResp $ do
        setStatusAndHeaders status headers
  when (hasBody status) $ 
    body (sendChunk servResp) (flush servResp)

sendChunk :: (a <: ServletResponse) => a -> Blaze.Builder -> IO ()
sendChunk resp builder = javaWith resp $ do
  buffSize <- getBufferSize
  let bs = toLazyByteString buffSize builder
  writeLazyByteString bs

flush :: (a <: ServletResponse) => a -> IO ()
flush resp = javaWith resp flushBuffer

setStatusAndHeaders :: HTTP.Status -> [HTTP.Header]
                    -> Java HttpServletResponse ()  
setStatusAndHeaders status headers = do
  setStatus $ HTTP.statusCode status
  forM_ headers $ \ (name,value) -> do
    setHeader (BSChar.unpack $ CI.original  name)
              (BSChar.unpack value)

hasBody :: HTTP.Status -> Bool
hasBody s = sc /= 204 && sc /= 304 && sc >= 200
  where sc = HTTP.statusCode s

toLazyByteString :: Int -> Blaze.Builder -> BSL.ByteString
toLazyByteString buffSize builder =
  Blaze.toLazyByteStringWith buffSize 0 buffSize builder BSL.empty

writeLazyByteString :: (a <: ServletResponse) => BSL.ByteString  -> Java a ()
writeLazyByteString BSLInt.Empty = return ()
writeLazyByteString (BSLInt.Chunk c cs) =
  writeStrictByteString c >> writeLazyByteString cs

writeStrictByteString :: (a <: ServletResponse) =>
                         BS.ByteString  -> Java a ()
writeStrictByteString bss = do
  bytes <- io getByteArray
  getOutputStream >- JIO.writeArrayOutputStream bytes
  where (fptr,offset,length) = BSInt.toForeignPtr bss
        getByteArray = withForeignPtr fptr $ \ ptr -> 
                         return $ toByteArray ptr offset length

sendRspFile :: Settings -> HTTP.Status
            -> HTTP.ResponseHeaders -> HTTP.RequestHeaders
            -> FilePath -> Maybe FilePart -> Bool
            -> Java HttpServletResponse ()
-- Sophisticated WAI applications.
-- We respect status. status MUST be a proper value.
sendRspFile settings status hdrs _ path (Just part) isHead = do
  let hdrs' = addContentHeadersForFilePart hdrs part
  sendRspFile2XX settings status hdrs' path part isHead
-- Simple WAI applications.
-- Status is ignored
sendRspFile settings _ hdrs reqhdrs path Nothing isHead = do
  efinfo <- io $ E.try $ getFileInfo path
  case efinfo of
    Left (_ex :: E.IOException) -> 
#ifdef WAI_SERVLET_DEBUG
      (io $ print _ex) >>
#endif
      sendRspFile404 hdrs
    Right finfo -> case conditionalRequest finfo hdrs reqhdrs of
      WithoutBody s         -> sendRspBuilder s hdrs mempty
      WithBody s hs part    -> sendRspFile2XX settings s hs
                                              path part isHead

sendRspFile2XX :: Settings -> HTTP.Status -> HTTP.ResponseHeaders
               -> FilePath -> FilePart -> Bool
               -> Java HttpServletResponse ()
sendRspFile2XX settings status hdrs path part isHead = do
  setStatusAndHeaders status hdrs
  unless isHead $ (getFileSender settings) path part  

sendRspFile404 :: HTTP.ResponseHeaders -> Java HttpServletResponse ()
sendRspFile404 hdrs = sendRspBuilder HTTP.notFound404 hdrs' body
  where hdrs' = replaceHeader HTTP.hContentType "text/plain; charset=utf-8" hdrs
        body = Blaze.fromByteString "File not found"

type HeaderValue = BS.ByteString

replaceHeader :: HTTP.HeaderName -> HeaderValue -> [HTTP.Header]
              -> [HTTP.Header]
replaceHeader k v hdrs = (k,v) : deleteBy ((==) `on` fst) (k,v) hdrs

