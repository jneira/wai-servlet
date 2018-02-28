{-# LANGUAGE FlexibleContexts, TypeOperators, CPP, ScopedTypeVariables #-}

module Network.Wai.Servlet.Settings
    ( Settings
    , CharEncoding (..)
    , defaultSettings
    , setUriEncoding
    , getUriEncoding
    , setFileSender
    , getFileSender
    , FilePart) where
import Network.Wai.Internal
import Java
#ifdef INTEROP
import qualified Interop.Java.IO as JIO
#else
import qualified Java.IO as JIO
#endif
import Javax.Servlet

foreign import java unsafe "@static network.wai.servlet.Utils.sendFile"
   sendFile :: (os <: JIO.OutputStream) =>
                os -> String -> Int64 -> Int64 -> Int64 -> Java a ()

data CharEncoding = ISO88591 | UTF8

type FileSender = FilePath -> FilePart -> Java HttpServletResponse ()

data Settings = Settings
  { settingUriEncoding :: CharEncoding
  , settingFileSender  :: FileSender }

defaultSettings :: Settings
defaultSettings = Settings
  { settingUriEncoding = UTF8
  , settingFileSender  = defaultFileSender }

setUriEncoding :: CharEncoding -> Settings -> Settings
setUriEncoding x y = y { settingUriEncoding = x }

getUriEncoding :: Settings -> CharEncoding
getUriEncoding = settingUriEncoding 

setFileSender :: FileSender -> Settings -> Settings
setFileSender x y = y { settingFileSender = x }

getFileSender :: Settings -> FileSender
getFileSender = settingFileSender

defaultFileSender :: FilePath -> FilePart -> Java HttpServletResponse ()
defaultFileSender path (FilePart off len size) = do
  os <- getOutputStream
  let [off',len',size'] = map fromIntegral [off,len,size]
  sendFile os path off' len' size'
