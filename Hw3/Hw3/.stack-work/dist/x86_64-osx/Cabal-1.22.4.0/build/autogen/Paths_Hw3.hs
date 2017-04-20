module Paths_Hw3 (
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
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/zhenlian/Desktop/HomeWork/230/Hw3/Hw3/.stack-work/install/x86_64-osx/nightly-2015-09-24/7.10.2/bin"
libdir     = "/Users/zhenlian/Desktop/HomeWork/230/Hw3/Hw3/.stack-work/install/x86_64-osx/nightly-2015-09-24/7.10.2/lib/x86_64-osx-ghc-7.10.2/Hw3-1.0-1q3tQIHY6iXAZtXZUroIGA"
datadir    = "/Users/zhenlian/Desktop/HomeWork/230/Hw3/Hw3/.stack-work/install/x86_64-osx/nightly-2015-09-24/7.10.2/share/x86_64-osx-ghc-7.10.2/Hw3-1.0"
libexecdir = "/Users/zhenlian/Desktop/HomeWork/230/Hw3/Hw3/.stack-work/install/x86_64-osx/nightly-2015-09-24/7.10.2/libexec"
sysconfdir = "/Users/zhenlian/Desktop/HomeWork/230/Hw3/Hw3/.stack-work/install/x86_64-osx/nightly-2015-09-24/7.10.2/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hw3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hw3_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Hw3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hw3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hw3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
