{-# LANGUAGE MagicHash,TypeFamilies, DataKinds #-}
module Lib where
import Java

data {-# CLASS "javax.servlet.GenericServlet" #-} GenericServlet =
  GenericServlet (Object# GenericServlet) 
data {-# CLASS "javax.servlet.ServletRequest" #-} ServletRequest =
  ServletRequest (Object# ServletRequest)
data {-# CLASS "javax.servlet.ServletResponse" #-} ServletResponse =
  ServletResponse (Object# ServletResponse)


data {-# CLASS "network.wai.servlet.WAIServlet" #-} WAIServlet =
  WAIServlet (Object# WAIServlet) deriving Class

type instance Inherits WAIServlet = '[GenericServlet]

service :: ServletResponse -> Java WAIServlet ()
service = undefined

foreign export java service :: ServletRequest -> ServletResponse ->
                               Java WAIServlet ()

