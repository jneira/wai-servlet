{-# LANGUAGE MagicHash, TypeFamilies, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, TypeOperators, BangPatterns,
             OverloadedStrings, StandaloneDeriving #-}
module Network.Wai.Servlet.File where

import Control.Exception as E
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B (pack)
import Data.ByteString (ByteString)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Network.HTTP.Date
import Network.Wai
import Numeric (showInt)
import Java
import Interop.Java.IO as JIO
-- Copied from https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/File.hs

-- | File information.
data FileInfo = FileInfo {
    fileInfoName :: !FilePath
  , fileInfoSize :: !Integer
  , fileInfoTime :: HTTPDate   -- ^ Modification time
  , fileInfoDate :: ByteString -- ^ Modification time in the GMT format
  } deriving (Eq, Show)

foreign import java unsafe "@new" newFile  :: String -> Java a File

getFileInfo :: FilePath -> IO FileInfo
getFileInfo path =  java $ do
  file <- newFile path
  withObject file $ do 
    regular <- fmap not isDirectory
    readable <- canRead
    if (regular && readable) then do
      time <- fmap (epochTimeToHTTPDate . fromIntegral) lastModified
      size <- fmap fromIntegral $ JIO.length
      let date = formatHTTPDate time
      return $ FileInfo { fileInfoName = path
                        , fileInfoSize = size
                        , fileInfoTime = time
                        , fileInfoDate = date }
    else io $ throwIO (userError "File:getInfo")

deriving instance Eq FilePart

data RspFileInfo = WithoutBody H.Status
                 | WithBody H.Status H.ResponseHeaders FilePart
                 deriving (Eq,Show)

conditionalRequest :: FileInfo
                   -> H.ResponseHeaders -> H.RequestHeaders
                   -> RspFileInfo
conditionalRequest finfo hs0 reqidx = case condition of
    nobody@(WithoutBody _) -> nobody
    WithBody s _ (FilePart off len size) ->
      let !hs = (H.hLastModified,date) : addContentHeaders hs0 off len size
      in WithBody s hs (FilePart off len size)
  where
    !mtime = fileInfoTime finfo
    !size  = fileInfoSize finfo
    !date  = fileInfoDate finfo
    !mcondition = ifmodified    reqidx size mtime
              <|> ifunmodified  reqidx size mtime
              <|> ifrange'       reqidx size mtime
    !condition = fromMaybe (unconditional reqidx size) mcondition

----------------------------------------------------------------

ifModifiedSince :: H.RequestHeaders -> Maybe HTTPDate
ifModifiedSince reqidx = lookup H.hIfModifiedSince reqidx >>= parseHTTPDate

ifUnmodifiedSince :: H.RequestHeaders -> Maybe HTTPDate
ifUnmodifiedSince reqidx = lookup H.hIfUnmodifiedSince reqidx >>= parseHTTPDate

ifRange :: H.RequestHeaders -> Maybe HTTPDate
ifRange reqidx =  lookup H.hIfRange reqidx >>= parseHTTPDate

----------------------------------------------------------------

ifmodified :: H.RequestHeaders -> Integer -> HTTPDate -> Maybe RspFileInfo
ifmodified reqidx size mtime = do
    date <- ifModifiedSince reqidx
    return $ if date /= mtime
             then unconditional reqidx size
             else WithoutBody H.notModified304

ifunmodified :: H.RequestHeaders -> Integer -> HTTPDate -> Maybe RspFileInfo
ifunmodified reqidx size mtime = do
    date <- ifUnmodifiedSince reqidx
    return $ if date == mtime
             then unconditional reqidx size
             else WithoutBody H.preconditionFailed412

ifrange' :: H.RequestHeaders -> Integer -> HTTPDate -> Maybe RspFileInfo
ifrange' reqidx size mtime = do
    date <- ifRange reqidx
    rng  <- lookup H.hRange reqidx
    return $ if date == mtime
             then parseRange rng size
             else WithBody H.ok200 [] (FilePart 0 size size)

unconditional :: H.RequestHeaders -> Integer -> RspFileInfo
unconditional reqidx size = case lookup H.hRange reqidx of
    Nothing  -> WithBody H.ok200 [] (FilePart 0 size size)
    Just rng -> parseRange rng size

----------------------------------------------------------------

parseRange :: ByteString -> Integer -> RspFileInfo
parseRange rng size = case H.parseByteRanges rng of
    Nothing    -> WithoutBody H.requestedRangeNotSatisfiable416
    Just []    -> WithoutBody H.requestedRangeNotSatisfiable416
    Just (r:_) -> let (!beg, !end) = checkRange r size
                      !len = end - beg + 1
                      s = if beg == 0 && end == size - 1 then
                              H.ok200
                            else
                              H.partialContent206
                  in WithBody s [] (FilePart beg len size)

checkRange :: H.ByteRange -> Integer -> (Integer, Integer)
checkRange (H.ByteRangeFrom   beg)     size = (beg, size - 1)
checkRange (H.ByteRangeFromTo beg end) size = (beg,  min (size - 1) end)
checkRange (H.ByteRangeSuffix count) size = (max 0 (size - count), size - 1)

----------------------------------------------------------------

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
