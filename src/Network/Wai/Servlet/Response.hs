{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             MultiParamTypeClasses,TypeOperators #-}
module Network.Wai.Servlet.Response where
import Control.Monad (forM_,when)
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
import Network.Wai.Servlet.Request
import Java
import qualified Java.IO as JIO

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
updateHttpServletResponse servReq servResp waiResp = case waiResp of

  (WaiIn.ResponseBuilder status headers builder) -> do
    withServResp $ do
      setStatusAndHeaders status headers
      buffSize <- getBufferSize
      when (hasBody status) $ 
           writeLazyByteString $ toLazyByteString buffSize builder
    return WaiIn.ResponseReceived

  (WaiIn.ResponseStream status headers body) -> do
    withServResp $ do
        setStatusAndHeaders status headers
    when (hasBody status) $ 
      body (sendChunk servResp) (flush servResp)
    return WaiIn.ResponseReceived

  respFile@(WaiIn.ResponseFile status headers filePath filePart) -> do
    withServResp $ do
      setStatusAndHeaders status headers
      serveFile respFile 
    return WaiIn.ResponseReceived
                                             
  (WaiIn.ResponseRaw rawStream response) ->
    error "ResponseRaw not supported by wai-servlet"
  where withServResp =  javaWith servResp
    
setStatusAndHeaders :: HTTP.Status -> [HTTP.Header] ->
                       Java HttpServletResponse ()  
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

sendChunk :: (a <: ServletResponse) => a -> Blaze.Builder -> IO ()
sendChunk resp builder = javaWith resp $ do
  buffSize <- getBufferSize
  let bs = toLazyByteString buffSize builder
  writeLazyByteString bs

flush :: (a <: ServletResponse) => a -> IO ()
flush resp = javaWith resp flushBuffer

serveFile :: (a <: ServletResponse) => WaiIn.Response -> Java a ()
serveFile = undefined
