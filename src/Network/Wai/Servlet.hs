{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             TypeOperators #-}
module Network.Wai.Servlet
    ( ServletApplication
    , GenericServletApplication
    , makeServiceMethod
    , DefaultWaiServlet
    , DefaultWaiServletApplication
    , makeServlet
    , module Network.Wai.Servlet.Request
    , module Network.Wai.Servlet.Response
    , module Network.Wai.Servlet.Settings ) where
import Network.Wai.Servlet.Response
import Network.Wai.Servlet.Request
import Network.Wai.Servlet.Settings
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import Java
import Javax.Servlet

type ServletApplication a = ServletRequest -> ServletResponse -> Java a ()
type GenericServletApplication = ServletApplication GenericServlet

-- Types for create a Servlet that can be used
-- to create war packages to deploy in j2ee servers
-- using "foreign export java service :: DefaultWaiServletApplication"
data {-# CLASS "network.wai.servlet.DefaultWaiServlet extends javax.servlet.GenericServlet" #-}
  DefaultWaiServlet = DefaultWaiServlet (Object# DefaultWaiServlet)
                    deriving Class
type instance Inherits DefaultWaiServlet = '[GenericServlet]
type DefaultWaiServletApplication = ServletApplication DefaultWaiServlet

makeServiceMethod :: (a <: GenericServlet) =>
  Wai.Application -> ServletApplication a
makeServiceMethod waiApp servReq servResp =
  do io $ waiApp waiReq waiRespond
     return ()
  where httpServReq = unsafeCast servReq
        httpServResp = unsafeCast servResp
        waiReq = makeWaiRequest httpServReq
        waiRespond = updateHttpServletResponse httpServReq httpServResp  

-- Make a proxy servlet to use programatically in embedded j2ee servers (tomcar,jetty)
foreign import java unsafe "@wrapper @abstract service"
   servlet :: GenericServletApplication -> GenericServlet

makeServlet ::  Wai.Application -> GenericServlet
makeServlet = servlet . makeServiceMethod

