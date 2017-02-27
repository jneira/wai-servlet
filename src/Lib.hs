{-# LANGUAGE MagicHash #-}

module Lib where

import Java

data {-# CLASS "javax.servlet.Servlet" #-} Servlet = Servlet (Object# Servlet)
data {-# CLASS "javax.servlet.ServletRequest" #-} ServletRequest =
  ServletRequest (Object# ServletRequest)
data {-# CLASS "javax.servlet.ServletResponse" #-} ServletResponse =
  ServletResponse (Object# ServletResponse)


foreign import java unsafe "@interface service" service
  :: Servlet -> ServletRequest -> ServletResponse -> IO ()

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
