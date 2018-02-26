{-# LANGUAGE MagicHash,TypeFamilies,DataKinds,FlexibleContexts,
             OverloadedStrings, TypeOperators,CPP #-}
module Javax.Servlet where

import Java
import Java.Array
#ifdef INTEROP
import qualified Interop.Java.IO as JIO
#else
import qualified Java.IO as JIO
#endif

-- Servlet

data {-# CLASS "javax.servlet.GenericServlet" #-} GenericServlet =
  GenericServlet (Object# GenericServlet) deriving Class

-- Request

data {-# CLASS "javax.servlet.ServletRequest" #-}
  ServletRequest = ServletRequest (Object# ServletRequest)
  deriving Class

data {-# CLASS "javax.servlet.http.HttpServletRequest" #-}
  HttpServletRequest = HttpServletRequest (Object# HttpServletRequest)
  deriving Class

data {-# CLASS "javax.servlet.ServletInputStream" #-}
  ServletInputStream = ServletInputStream (Object# ServletInputStream)
  deriving Class

type instance Inherits HttpServletRequest = '[ServletRequest]
type instance Inherits ServletInputStream = '[JIO.InputStream]

foreign import java unsafe "@interface getCharacterEncoding" getCharacterEncoding ::
  (a <: ServletRequest) => Java a String
foreign import java unsafe "@interface getProtocol" getProtocol ::
  (a <: ServletRequest) => Java a String
foreign import java unsafe "@interface isSecure" isSecure ::
  (a <: ServletRequest) => Java a Bool
foreign import java unsafe "@interface getRemoteAddr" getRemoteAddr ::
  (a <: ServletRequest) => Java a String
foreign import java unsafe "@interface getRemotePort" getRemotePort ::
  (a <: ServletRequest) => Java a Int
foreign import java unsafe "@interface getInputStream" getInputStream ::
  (a <: ServletRequest) => Java a ServletInputStream
foreign import java unsafe "@interface getContentLength" getContentLength ::
  (a <: ServletRequest) => Java a Int

foreign import java unsafe "@interface getMethod" getMethod ::
  (a <: HttpServletRequest) => Java a String
foreign import java unsafe "@interface getPathInfo" getPathInfo ::
  (a <: HttpServletRequest) => Java a (Maybe String)
foreign import java unsafe "@interface getQueryString" getQueryString ::
  (a <: HttpServletRequest) => Java a (Maybe String)
foreign import java unsafe "@interface getHeaderNames" getHeaderNames ::
  (a <: HttpServletRequest) => Java a (Enumeration JString)
foreign import java unsafe "@interface getHeaders" getHeaders ::
  (a <: HttpServletRequest) => String -> Java a (Maybe (Enumeration JString))

-- Response

data {-# CLASS "javax.servlet.ServletResponse" #-}
  ServletResponse = ServletResponse (Object# ServletResponse)
  deriving Class

data {-# CLASS "javax.servlet.http.HttpServletResponse" #-}
  HttpServletResponse = HttpServletResponse (Object# HttpServletResponse)
  deriving Class

type instance Inherits HttpServletResponse = '[ServletResponse]

data {-# CLASS "javax.servlet.ServletOutputStream" #-}
  ServletOutputStream = ServletOutputStream (Object# ServletOutputStream)
  deriving Class

type instance Inherits ServletOutputStream = '[JIO.OutputStream]

foreign import java unsafe "@interface setStatus" setStatus ::
   Int -> Java HttpServletResponse ()
foreign import java unsafe "@interface setHeader" setHeader ::
   String -> String -> Java HttpServletResponse ()
foreign import java unsafe "@interface getOutputStream" getOutputStream ::
   (a <: ServletResponse) => Java a ServletOutputStream
foreign import java unsafe "@interface flushBuffer" flushBuffer ::
   (a <: ServletResponse) => Java a ()
foreign import java unsafe "@interface getBufferSize" getBufferSize ::
   (a <: ServletResponse) => Java a Int
