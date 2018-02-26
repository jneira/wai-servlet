module Network.Wai.Servlet.Settings
    ( Settings
    , CharEncoding (..)
    , defaultSettings
    , setUriEncoding
    , getUriEncoding ) where

data CharEncoding = ISO88591 | UTF8

data Settings = Settings
  { settingUriEncoding :: CharEncoding }

defaultSettings :: Settings
defaultSettings = Settings { settingUriEncoding = UTF8 }

setUriEncoding :: CharEncoding -> Settings -> Settings
setUriEncoding x y = y { settingUriEncoding = x }

getUriEncoding :: Settings -> CharEncoding
getUriEncoding = settingUriEncoding 

