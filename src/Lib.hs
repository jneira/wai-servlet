{-# LANGUAGE MagicHash,TypeFamilies, DataKinds #-}
module Lib where
import Java

data {-# CLASS "javax.servlet.GenericServlet" #-} GenericServlet =
  GenericServlet (Object# GenericServlet) 
data {-# CLASS "javax.servlet.ServletRequest" #-} ServletRequest =
  ServletRequest (Object# ServletRequest)
data {-# CLASS "javax.servlet.ServletResponse" #-} ServletResponse =
  ServletResponse (Object# ServletResponse)

foreign import java unsafe "@wrapper @abstract service "
  service :: (ServletRequest -> ServletResponse -> Java GenericServlet()) -> GenericServlet 

data {-# CLASS "network.wai.servlet.WAIServlet extends javax.servlet.GenericServlet" #-}
  WAIServlet = WAIServlet (Object# WAIServlet) deriving Class

type instance Inherits WAIServlet = '[GenericServlet]

service1 :: ServletRequest -> ServletResponse -> Java WAIServlet ()
service1 = undefined

foreign export java service1 :: ServletRequest -> ServletResponse ->
                                Java WAIServlet ()
