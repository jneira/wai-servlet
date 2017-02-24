module Paths_wai_servlet (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Documents and Settings\\gbd\\Datos de programa\\epm\\bin"
libdir     = "C:\\Documents and Settings\\gbd\\Datos de programa\\epm\\i386-windows-eta-0.0.5-ghc7_10_3\\wai-servlet-0.1.0.0"
datadir    = "C:\\Documents and Settings\\gbd\\Datos de programa\\epm\\i386-windows-eta-0.0.5-ghc7_10_3\\wai-servlet-0.1.0.0"
libexecdir = "C:\\Documents and Settings\\gbd\\Datos de programa\\epm\\wai-servlet-0.1.0.0"
sysconfdir = "C:\\Documents and Settings\\gbd\\Datos de programa\\epm\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wai_servlet_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wai_servlet_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "wai_servlet_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wai_servlet_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wai_servlet_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
