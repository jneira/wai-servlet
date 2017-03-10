{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts #-}
module Network.Wai.Servlet.Request where
import qualified Network.Wai as Wai
import Java

data {-# CLASS "javax.servlet.ServletRequest" #-} ServletRequest =
  ServletRequest (Object# ServletRequest) deriving Class

data {-# CLASS "javax.servlet.http.HttpServletRequest" #-} HttpServletRequest =
  HttpServletRequest (Object# HttpServletRequest) deriving Class

type instance Inherits HttpServletRequest = '[ServletRequest]

makeWaiRequest :: HttpServletRequest -> Wai.Request
makeWaiRequest req = undefined

