{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             OverloadedStrings,MultiParamTypeClasses #-}
module Network.Wai.Servlet.Response where
import Control.Monad (forM_,when)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.CaseInsensitive as CI (original)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as BSChar (unpack)
import Data.Word
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiIn
import qualified Network.HTTP.Types as HTTP
import Java

data {-# CLASS "javax.servlet.ServletResponse" #-} ServletResponse =
  ServletResponse (Object# ServletResponse) deriving Class

data {-# CLASS "javax.servlet.HttpServletResponse" #-} HttpServletResponse =
  HttpServletResponse (Object# HttpServletResponse) deriving Class

type instance Inherits HttpServletResponse = '[ServletResponse]

data {-# CLASS "java.io.OutputStream" #-} OutputStream =
  OutputStream (Object# OutputStream) deriving Class

data {-# CLASS "javax.servlet.ServletOutputStream" #-} ServletOutputStream =
  ServletOutputStream (Object# ServletOutputStream) deriving Class

type instance Inherits ServletOutputStream = '[OutputStream]

foreign import java unsafe setStatus ::  Int -> Java HttpServletResponse ()
foreign import java unsafe setHeader ::  String -> String ->
                                         Java HttpServletResponse ()
foreign import java unsafe getOutputStream :: Extends a ServletResponse  =>
                                              Java a ServletOutputStream
foreign import java unsafe flushBuffer :: Extends a ServletResponse => Java a ()

foreign import java unsafe write :: Extends a OutputStream  => Int -> Java a () 
foreign import java unsafe "write" writeByteArray :: Extends a OutputStream  =>
                                                     JByteArray -> Int -> Int  ->
                                                     Java a () 

setStatusAndHeaders :: HTTP.Status -> [HTTP.Header] ->
                       Java HttpServletResponse ()  
setStatusAndHeaders status headers = do
  setStatus $ HTTP.statusCode status
  forM_ headers $ \ (name,value) -> do
    setHeader (BSChar.unpack $ CI.original  name)
              (BSChar.unpack value)

writeByteString :: Extends a ServletResponse  => BS.ByteString  -> Java a ()
writeByteString bs  =
  case unc of
    Nothing -> return ()
    Just (h,t) -> do
      getOutputStream >- write (fromIntegral h)
      writeByteString t
  where unc = BS.uncons bs
  
instance JavaConverter BS.ByteString JByteArray where
  toJava = toJava . map fromIntegral . BS.unpack
  fromJava = BS.pack . map fromIntegral . fromJava

updateHttpServletResponse :: HttpServletResponse -> Wai.Response ->
                         IO Wai.ResponseReceived
updateHttpServletResponse servResp waiResp = case waiResp of
  (WaiIn.ResponseFile status headers filePath filePart) -> undefined
  (WaiIn.ResponseBuilder status headers builder) -> do
    withServResp $ do
      setStatusAndHeaders status headers
      when (hasBody status) $ 
           writeByteString $ Blaze.toLazyByteString builder
    return WaiIn.ResponseReceived
  (WaiIn.ResponseStream status headers body) -> do
    withServResp $ do
      setStatusAndHeaders status headers
    when (hasBody status) $ 
      body (sendChunk servResp) (flush servResp)
    return WaiIn.ResponseReceived
  (WaiIn.ResponseRaw rawStream response) -> undefined
  where withServResp =  javaWith servResp
    

hasBody :: HTTP.Status -> Bool
hasBody s = sc /= 204 && sc /= 304 && sc >= 200
  where sc = HTTP.statusCode s

sendChunk :: Extends a ServletResponse => a -> Blaze.Builder -> IO ()
sendChunk resp = javaWith resp . writeByteString . Blaze.toLazyByteString 

flush :: Extends a ServletResponse => a -> IO ()
flush resp = do
  javaWith resp flushBuffer
