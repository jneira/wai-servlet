{-# LANGUAGE MagicHash,TypeFamilies, DataKinds #-}
module Network.Wai.Servlet where
import import Network.Wai (application,Application)
import Java

data {-# CLASS "javax.servlet.GenericServlet" #-} GenericServlet =
  GenericServlet (Object# GenericServlet) 
data {-# CLASS "javax.servlet.ServletRequest" #-} ServletRequest =
  ServletRequest (Object# ServletRequest)
data {-# CLASS "javax.servlet.ServletResponse" #-} ServletResponse =
  ServletResponse (Object# ServletResponse)

type ServletApp = ServletRequest -> ServletResponse -> Java GenericServlet()

foreign import java unsafe "@wrapper @abstract service"
   serviceMethod :: ServletApp -> GenericServlet 

makeServiceMethod :: Application -> ServletRequest -> ServletResponse
makeServiceMethod  app servReq servResp = undefined 

makeWaiRequest :: ServletRequest -> Request
makeWaiRequest = undefined

makeWaiResponse :: ServletResponse -> Response
makeWaiResponse = undefined
  
data {-# CLASS "network.wai.servlet.WAIServlet extends javax.servlet.GenericServlet" #-}
  WAIServlet = WAIServlet (Object# WAIServlet) deriving Class

type instance Inherits WAIServlet = '[GenericServlet]

service:: ServletRequest -> ServletResponse -> Java WAIServlet ()
service req resp = do io $ putStrLn "hello"

foreign export java service :: ServletRequest -> ServletResponse -> Java WAIServlet ()

