{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts #-}
module Network.Wai.Servlet where
import Network.Wai.Servlet.Response
import Network.Wai.Servlet.Request
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import Java

data {-# CLASS "javax.servlet.GenericServlet" #-} GenericServlet =
  GenericServlet (Object# GenericServlet)  deriving Class

type ServletApplication a = ServletRequest -> ServletResponse -> Java a ()
type GenericServletApplication = ServletApplication GenericServlet

makeServiceMethod :: Extends a GenericServlet => Wai.Application -> ServletApplication a
makeServiceMethod  waiApp servReq servResp =
  do _ <- io $ waiApp waiReq waiRespond
     return ()
  where waiReq = makeWaiRequest $ unsafeCast servReq
        waiRespond = updateHttpServletResponse $ unsafeCast servResp  

-- Types for create a Servlet which can be used for create war packages to deploy in j2ee servers
-- using "foreign export java service :: WaiServletApplication"
data {-# CLASS "network.wai.servlet.WaiServlet extends javax.servlet.GenericServlet" #-}
  WaiServlet = WaiServlet (Object# WaiServlet) deriving Class

type instance Inherits WaiServlet = '[GenericServlet]

type WaiServletApplication = ServletApplication WaiServlet


-- Make a proxy servlet to use programatically in embedded j2ee servers (tomcar,jetty)
foreign import java unsafe "@wrapper @abstract service"
   servlet :: GenericServletApplication -> GenericServlet

makeServlet ::  Wai.Application -> GenericServlet
makeServlet = servlet . makeServiceMethod

