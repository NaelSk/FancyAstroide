{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_project01 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/38573f223eca1d58fb4349b5aad931a9506617d3d2f6dc5cd66bae038f314ea5/8.6.5/bin"
libdir     = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/38573f223eca1d58fb4349b5aad931a9506617d3d2f6dc5cd66bae038f314ea5/8.6.5/lib/x86_64-linux-ghc-8.6.5/project01-0.1.0.0-CZ13xEnkd6hFJiQVy3Wha-project01"
dynlibdir  = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/38573f223eca1d58fb4349b5aad931a9506617d3d2f6dc5cd66bae038f314ea5/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/38573f223eca1d58fb4349b5aad931a9506617d3d2f6dc5cd66bae038f314ea5/8.6.5/share/x86_64-linux-ghc-8.6.5/project01-0.1.0.0"
libexecdir = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/38573f223eca1d58fb4349b5aad931a9506617d3d2f6dc5cd66bae038f314ea5/8.6.5/libexec/x86_64-linux-ghc-8.6.5/project01-0.1.0.0"
sysconfdir = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/38573f223eca1d58fb4349b5aad931a9506617d3d2f6dc5cd66bae038f314ea5/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "project01_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "project01_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "project01_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "project01_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project01_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project01_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
