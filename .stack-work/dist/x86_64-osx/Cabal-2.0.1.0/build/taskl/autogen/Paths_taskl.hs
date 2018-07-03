{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_taskl (
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

bindir     = "/Users/johnpolo/Documents/Programming/Haskell/MyPrograms/Taskl/.stack-work/install/x86_64-osx/lts-11.14/8.2.2/bin"
libdir     = "/Users/johnpolo/Documents/Programming/Haskell/MyPrograms/Taskl/.stack-work/install/x86_64-osx/lts-11.14/8.2.2/lib/x86_64-osx-ghc-8.2.2/taskl-0.1.0.0-8hDhJVuV2pp4cFbr8FPsGf-taskl"
dynlibdir  = "/Users/johnpolo/Documents/Programming/Haskell/MyPrograms/Taskl/.stack-work/install/x86_64-osx/lts-11.14/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/johnpolo/Documents/Programming/Haskell/MyPrograms/Taskl/.stack-work/install/x86_64-osx/lts-11.14/8.2.2/share/x86_64-osx-ghc-8.2.2/taskl-0.1.0.0"
libexecdir = "/Users/johnpolo/Documents/Programming/Haskell/MyPrograms/Taskl/.stack-work/install/x86_64-osx/lts-11.14/8.2.2/libexec/x86_64-osx-ghc-8.2.2/taskl-0.1.0.0"
sysconfdir = "/Users/johnpolo/Documents/Programming/Haskell/MyPrograms/Taskl/.stack-work/install/x86_64-osx/lts-11.14/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "taskl_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "taskl_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "taskl_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "taskl_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "taskl_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "taskl_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
