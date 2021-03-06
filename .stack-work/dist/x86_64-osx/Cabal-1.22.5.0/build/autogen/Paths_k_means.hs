module Paths_k_means (
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

bindir     = "/Users/dp/haskell-projects/k-means/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/bin"
libdir     = "/Users/dp/haskell-projects/k-means/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/lib/x86_64-osx-ghc-7.10.3/k-means-0.1.0.0-IrNdTTHds287x4vAObvtap"
datadir    = "/Users/dp/haskell-projects/k-means/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/share/x86_64-osx-ghc-7.10.3/k-means-0.1.0.0"
libexecdir = "/Users/dp/haskell-projects/k-means/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/libexec"
sysconfdir = "/Users/dp/haskell-projects/k-means/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "k_means_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "k_means_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "k_means_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "k_means_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "k_means_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
