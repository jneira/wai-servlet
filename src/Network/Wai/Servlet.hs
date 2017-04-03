{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             TypeOperators #-}
module Network.Wai.Servlet
    ( ServletApplication
    , GenericServletApplication
    ,  ServletAppSettings ()
    , defaultServletAppSettings
    , makeServiceMethod
    , makeServiceMethodWithSettings
    , DefaultWaiServlet
    , DefaultWaiServletApplication
    , makeServlet
    ) where
import Network.Wai.Servlet.Response
import Network.Wai.Servlet.Request
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import Java

data {-# CLASS "javax.servlet.GenericServlet" #-} GenericServlet =
  GenericServlet (Object# GenericServlet) deriving Class

type ServletApplication a = ServletRequest -> ServletResponse -> Java a ()
type GenericServletApplication = ServletApplication GenericServlet

data ServletAppSettings = ServletAppSettings
  { requestSettings :: RequestSettings }

defaultServletAppSettings :: ServletAppSettings
defaultServletAppSettings = ServletAppSettings
  { requestSettings = defaultRequestSettings }

makeServiceMethod :: (a <: GenericServlet) =>
                     Wai.Application -> ServletApplication a
makeServiceMethod  =
  makeServiceMethodWithSettings defaultServletAppSettings

makeServiceMethodWithSettings :: (a <: GenericServlet) =>
  ServletAppSettings -> Wai.Application -> ServletApplication a
makeServiceMethodWithSettings settings waiApp servReq servResp =
  do io $ waiApp waiReq waiRespond
     return ()
  where httpServReq = unsafeCast servReq
        httpServResp = unsafeCast servResp
        reqSettings = requestSettings settings
        waiReq = makeWaiRequestWithSettings reqSettings httpServReq
        waiRespond = updateHttpServletResponse httpServReq httpServResp  

-- Types for create a Servlet that can be used for create war packages to deploy in j2ee servers
-- using "foreign export java service :: DefaultWaiServletApplication"
data {-# CLASS "network.wai.servlet.DefaultWaiServlet extends javax.servlet.GenericServlet" #-}
  DefaultWaiServlet = DefaultWaiServlet (Object# DefaultWaiServlet)
                    deriving Class

type instance Inherits DefaultWaiServlet = '[GenericServlet]

type DefaultWaiServletApplication = ServletApplication DefaultWaiServlet


-- Make a proxy servlet to use programatically in embedded j2ee servers (tomcar,jetty)
foreign import java unsafe "@wrapper @abstract service"
   servlet :: GenericServletApplication -> GenericServlet

makeServlet ::  Wai.Application -> GenericServlet
makeServlet = servlet . makeServiceMethod

