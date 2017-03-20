{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             OverloadedStrings, TypeOperators #-}
module Network.Wai.Servlet.Request where
import Network.Wai.Internal
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr (SockAddrInet))
import qualified Data.ByteString as B
import Java

data {-# CLASS "javax.servlet.ServletRequest" #-}
  ServletRequest = ServletRequest (Object# ServletRequest)
  deriving Class

data {-# CLASS "javax.servlet.http.HttpServletRequest" #-}
  HttpServletRequest = HttpServletRequest (Object# HttpServletRequest)
  deriving Class

foreign import java unsafe "@interface getMethod" getMethod ::
  (a <: HttpServletRequest) => Java a String

type instance Inherits HttpServletRequest = '[ServletRequest]

makeWaiRequest :: HttpServletRequest -> Request
makeWaiRequest servReq req = pureJavaWith servReq $ Request
   { requestMethod = getMethod
   , httpVersion = H.http10
   , rawPathInfo = B.empty
   , rawQueryString = B.empty
   , requestHeaders = []
   , isSecure = False
   , remoteHost = SockAddrInet 0 0
   , pathInfo = []
   , queryString = []
   , requestBody = return B.empty
   , vault = mempty
   , requestBodyLength = KnownLength 0
   , requestHeaderHost = Nothing
   , requestHeaderRange = Nothing
   , requestHeaderReferer = Nothing
   , requestHeaderUserAgent = Nothing
  }

