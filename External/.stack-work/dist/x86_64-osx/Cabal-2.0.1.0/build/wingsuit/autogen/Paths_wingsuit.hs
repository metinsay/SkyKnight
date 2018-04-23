{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_wingsuit (
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
version = Version [0,0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/drew/SkyKnight/External/.stack-work/install/x86_64-osx/lts-11.4/8.2.2/bin"
libdir     = "/Users/drew/SkyKnight/External/.stack-work/install/x86_64-osx/lts-11.4/8.2.2/lib/x86_64-osx-ghc-8.2.2/wingsuit-0.0.0.0-4y6m97tk4yx2uElZGctSRW-wingsuit"
dynlibdir  = "/Users/drew/SkyKnight/External/.stack-work/install/x86_64-osx/lts-11.4/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/drew/SkyKnight/External/.stack-work/install/x86_64-osx/lts-11.4/8.2.2/share/x86_64-osx-ghc-8.2.2/wingsuit-0.0.0.0"
libexecdir = "/Users/drew/SkyKnight/External/.stack-work/install/x86_64-osx/lts-11.4/8.2.2/libexec/x86_64-osx-ghc-8.2.2/wingsuit-0.0.0.0"
sysconfdir = "/Users/drew/SkyKnight/External/.stack-work/install/x86_64-osx/lts-11.4/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wingsuit_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wingsuit_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wingsuit_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wingsuit_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wingsuit_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wingsuit_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
