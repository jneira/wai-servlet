{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Network.Wai.Servlet.File where

import qualified Data.ByteString.Char8 as B (pack)
import Data.ByteString (ByteString)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Network.Wai
import Numeric (showInt)

-- Copied from https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/File.hs

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
