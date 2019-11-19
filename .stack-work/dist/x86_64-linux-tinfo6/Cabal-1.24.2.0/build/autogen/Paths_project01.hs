{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
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

bindir     = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/759118837551497a182132bb26169095144c04f4e9d31734537bd1d184a9eb19/8.0.2/bin"
libdir     = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/759118837551497a182132bb26169095144c04f4e9d31734537bd1d184a9eb19/8.0.2/lib/x86_64-linux-ghc-8.0.2/project01-0.1.0.0"
dynlibdir  = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/759118837551497a182132bb26169095144c04f4e9d31734537bd1d184a9eb19/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/759118837551497a182132bb26169095144c04f4e9d31734537bd1d184a9eb19/8.0.2/share/x86_64-linux-ghc-8.0.2/project01-0.1.0.0"
libexecdir = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/759118837551497a182132bb26169095144c04f4e9d31734537bd1d184a9eb19/8.0.2/libexec"
sysconfdir = "/home/nael/Documents/FB2/project01/.stack-work/install/x86_64-linux-tinfo6/759118837551497a182132bb26169095144c04f4e9d31734537bd1d184a9eb19/8.0.2/etc"

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
