{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             MultiParamTypeClasses,TypeOperators,RecordWildCards,
             OverloadedStrings,CPP,ScopedTypeVariables #-}

module Network.Wai.Servlet.Response
    ( HttpServletResponse
    , ServletResponse
    , updateHttpServletResponse ) where
import Control.Monad (forM_,when)
import Control.Exception as E
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.CaseInsensitive as CI (original)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSLInt
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSInt
import qualified Data.ByteString.Char8 as BSChar (unpack,pack)
import Foreign.ForeignPtr (ForeignPtr,withForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiIn
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Network.Wai.Servlet.Request
import Network.Wai.Servlet.File
import Java
#ifdef INTEROP
import qualified Interop.Java.IO as JIO
#else
import qualified Java.IO as JIO
#endif
import Java.Exception

data {-# CLASS "javax.servlet.ServletResponse" #-}
  ServletResponse = ServletResponse (Object# ServletResponse)
  deriving Class

data {-# CLASS "javax.servlet.http.HttpServletResponse" #-}
  HttpServletResponse = HttpServletResponse (Object# HttpServletResponse)
  deriving Class

type instance Inherits HttpServletResponse = '[ServletResponse]

data {-# CLASS "javax.servlet.ServletOutputStream" #-}
  ServletOutputStream = ServletOutputStream (Object# ServletOutputStream)
  deriving Class

type instance Inherits ServletOutputStream = '[JIO.OutputStream]

foreign import java unsafe "@interface setStatus" setStatus ::
   Int -> Java HttpServletResponse ()
foreign import java unsafe "@interface setHeader" setHeader ::
   String -> String -> Java HttpServletResponse ()
foreign import java unsafe "@interface getOutputStream" getOutputStream ::
   (a <: ServletResponse) => Java a ServletOutputStream
foreign import java unsafe "@interface flushBuffer" flushBuffer ::
   (a <: ServletResponse) => Java a ()
foreign import java unsafe "@interface getBufferSize" getBufferSize ::
   (a <: ServletResponse) => Java a Int

updateHttpServletResponse :: HttpServletRequest -> HttpServletResponse ->
                             Wai.Response -> IO Wai.ResponseReceived
updateHttpServletResponse servReq servResp waiResp = javaWith servResp $ do
  case waiResp of
    (WaiIn.ResponseBuilder status headers builder) ->
      sendRspBuilder status headers builder

    (WaiIn.ResponseStream status headers body) -> 
      io $ sendRspStream servResp status headers body 

    (WaiIn.ResponseFile status headers filePath filePart) -> do
      let isHead = requestMethod servReq == HTTP.methodHead 
      sendRspFile status headers (requestHeaders servReq) filePath filePart isHead
                                             
    (WaiIn.ResponseRaw rawStream response) ->
      error "ResponseRaw not supported by wai-servlet"
  return WaiIn.ResponseReceived

sendRspBuilder :: HTTP.Status -> [HTTP.Header] -> Blaze.Builder ->
                  Java HttpServletResponse ()  
sendRspBuilder status headers builder = do
  setStatusAndHeaders status headers
  buffSize <- getBufferSize
  when (hasBody status) $ 
    writeLazyByteString $ toLazyByteString buffSize builder

sendRspStream :: HttpServletResponse -> HTTP.Status -> [HTTP.Header] -> WaiIn.StreamingBody ->
                 IO ()  
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

setStatusAndHeaders :: HTTP.Status -> [HTTP.Header] -> Java HttpServletResponse ()  
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

foreign import java unsafe "@static network.wai.servlet.Utils.toByteArray"
   toByteArray :: Ptr Word8 -> Int -> Int -> JByteArray

writeStrictByteString :: (a <: ServletResponse) =>
                         BS.ByteString  -> Java a ()
writeStrictByteString bss = do
  bytes <- io getByteArray
  getOutputStream >- JIO.writeArrayOutputStream bytes
  where (fptr,offset,length) = BSInt.toForeignPtr bss
        getByteArray = withForeignPtr fptr $ \ ptr -> 
                         return $ toByteArray ptr offset length

sendRspFile :: HTTP.Status -> HTTP.ResponseHeaders -> HTTP.RequestHeaders -> FilePath ->
               Maybe WaiIn.FilePart -> Bool -> Java HttpServletResponse ()
-- Sophisticated WAI applications.
-- We respect status. status MUST be a proper value.
sendRspFile status hdrs _ path (Just part) isHead = do
  let hdrs' = addContentHeadersForFilePart hdrs part
  sendRspFile2XX status hdrs' path part isHead
-- Simple WAI applications.
-- Status is ignored
sendRspFile _ hdrs reqhdrs path Nothing isHead = do
  efinfo <- io $ E.try $ getFileInfo path
  case efinfo of
    Left (_ex :: JException) -> 
#ifdef WAI_SERVLET_DEBUG
      io $ print _ex >>
#endif
      sendRspFile404 hdrs
    Right finfo -> case conditionalRequest finfo hdrs reqhdrs of
      WithoutBody s         -> sendRspBuilder s hdrs mempty
      WithBody s hs part    -> sendRspFile2XX s hs path part isHead

sendRspFile2XX :: HTTP.Status -> HTTP.ResponseHeaders -> FilePath ->
                WaiIn.FilePart -> Bool -> Java HttpServletResponse ()
sendRspFile2XX status hdrs path (WaiIn.FilePart off len size) isHead = do
  setStatusAndHeaders status hdrs
  when isHead $ do
    os <- getOutputStream
    let [off',len',size'] = map fromIntegral [off,len,size]
    sendFile os path off' len' size'

sendRspFile404 :: HTTP.ResponseHeaders -> Java HttpServletResponse ()
sendRspFile404 = undefined

foreign import java unsafe "@static network.wai.servlet.Utils.sendFile"
   sendFile :: (os <: JIO.OutputStream) =>
                os -> String -> Int64 -> Int64 -> Int64 -> Java a ()

