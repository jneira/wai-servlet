{-# LANGUAGE FlexibleContexts, TypeOperators, CPP, ScopedTypeVariables #-}

module Network.Wai.Servlet.Settings
    ( Settings
    , CharEncoding (..)
    , defaultSettings
    , settingsFromSysProps
    , setUriEncoding
    , getUriEncoding
    , setFileSender
    , getFileSender
    , FilePart) where
import Data.Char (toUpper)
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
foreign import java unsafe "@static System.getProperty"
   getSystemProperty :: String -> Java a (Maybe String)

data CharEncoding = ISO88591 | UTF8
  deriving (Show, Read)

type FileSender = FilePath -> FilePart -> Java HttpServletResponse ()

data Settings = Settings
  { settingUriEncoding :: CharEncoding
  , settingFileSender  :: FileSender }

defaultSettings :: Settings
defaultSettings = Settings
  { settingUriEncoding = UTF8
  , settingFileSender  = defaultFileSender }

readCharEncoding :: String -> CharEncoding
readCharEncoding str =
  case map toUpper str of
    "ISO88591"   -> ISO88591 -- haskell
    "ISO-8859-1" -> ISO88591 -- java.nio
    "ISO8859_1"  -> ISO88591 -- java.io
    "UTF8"      -> UTF8
    "UTF-8"       -> UTF8
    _            -> error $ "Bad input for CharEncoding: " ++ str
    
settingsFromSysProps :: Java a Settings
settingsFromSysProps = do
  let settings = defaultSettings
  mbEncStr <- getSystemProperty "network.wai.servlet.settings.uriEncoding"
  case mbEncStr of
    Nothing     -> return settings
    Just encStr -> do
       let enc = readCharEncoding encStr
       return $ setUriEncoding enc settings

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
