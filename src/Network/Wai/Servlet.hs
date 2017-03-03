{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts #-}
module Network.Wai.Servlet where
import Network.Wai as Wai
import Java

data {-# CLASS "javax.servlet.GenericServlet" #-} GenericServlet =
  GenericServlet (Object# GenericServlet)  deriving Class
data {-# CLASS "javax.servlet.ServletRequest" #-} ServletRequest =
  ServletRequest (Object# ServletRequest) deriving Class
data {-# CLASS "javax.servlet.ServletResponse" #-} ServletResponse =
  ServletResponse (Object# ServletResponse) deriving Class

type ServletApplication a = ServletRequest -> ServletResponse -> Java a ()
type GenericServletApplication = ServletApplication GenericServlet

makeServiceMethod :: Extends a GenericServlet => Wai.Application -> ServletApplication a
makeServiceMethod  waiApp servReq servResp =
  do _ <- io $ waiApp waiReq waiRespond
     return ()
  where waiReq = makeWaiRequest servReq
        waiRespond = updateServletResponse servResp  

makeWaiRequest :: ServletRequest -> Wai.Request
makeWaiRequest = undefined

updateServletResponse :: ServletResponse -> Wai.Response -> IO Wai.ResponseReceived
updateServletResponse servResp waiResp = undefined

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

