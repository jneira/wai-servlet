{-# LANGUAGE MagicHash, TypeFamilies, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, TypeOperators, BangPatterns,
             OverloadedStrings #-}
module Network.Wai.Servlet.File where

import Control.Exception as E
import qualified Data.ByteString.Char8 as B (pack)
import Data.ByteString (ByteString)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Network.HTTP.Date
import Network.Wai
import Numeric (showInt)
import Java
import Java.IO
-- Copied from https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/File.hs

-- | File information.
data FileInfo = FileInfo {
    fileInfoName :: !FilePath
  , fileInfoSize :: !Integer
  , fileInfoTime :: HTTPDate   -- ^ Modification time
  , fileInfoDate :: ByteString -- ^ Modification time in the GMT format
  } deriving (Eq, Show)

foreign import java unsafe "@new" newFile  :: String -> Java a File

getInfo :: FilePath -> IO FileInfo
getInfo path =  java $ do
  file <- newFile path
  withObject file $ do 
    regular <- fmap not isDirectory
    readable <- canRead
    if (regular && readable) then do
      time <- fmap (epochTimeToHTTPDate . fromIntegral) lastModified
      size <- fmap fromIntegral $ Java.IO.length
      let date = formatHTTPDate time
      return $ FileInfo { fileInfoName = path
                        , fileInfoSize = size
                        , fileInfoTime = time
                        , fileInfoDate = date }
    else io $ throwIO (userError "File:getInfo")

data RspFileInfo = WithoutBody H.Status
                 | WithBody H.Status H.ResponseHeaders Integer Integer
                 deriving (Eq,Show)

conditionalRequest :: FileInfo
                   -> H.ResponseHeaders -> [Header]
                   -> RspFileInfo
conditionalRequest = undefined

contentRangeHeader :: Integer -> Integer -> Integer -> H.Header
contentRangeHeader beg end total = (H.hContentRange, range)
  where
    range = B.pack
      -- building with ShowS
      $ 'b' : 'y': 't' : 'e' : 's' : ' '
      : (if beg > end then ('*':) else
          showInt beg
          . ('-' :)
          . showInt end)
      ( '/'
      : showInt total "")

addContentHeaders :: H.ResponseHeaders -> Integer -> Integer -> Integer  ->
                     H.ResponseHeaders
addContentHeaders hs off len size
  | len == size = hs'
  | otherwise   = let !ctrng = contentRangeHeader off (off + len - 1) size
                  in ctrng:hs'
  where
    !lengthBS = packIntegral len
    !hs' = (H.hContentLength, lengthBS) : (H.hAcceptRanges,"bytes") : hs
    packIntegral =  B.pack . show

addContentHeadersForFilePart :: H.ResponseHeaders -> FilePart -> H.ResponseHeaders
addContentHeadersForFilePart hs part = addContentHeaders hs off len size
  where
    off = filePartOffset part
    len = filePartByteCount part
    size = filePartFileSize part
