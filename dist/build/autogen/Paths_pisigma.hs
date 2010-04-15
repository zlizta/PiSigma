module Paths_pisigma (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,0,3], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/txa/.cabal/bin"
libdir     = "/Users/txa/.cabal/lib/pisigma-0.1.0.3/ghc-6.12.1"
datadir    = "/Users/txa/.cabal/share/pisigma-0.1.0.3"
libexecdir = "/Users/txa/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "pisigma_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "pisigma_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "pisigma_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "pisigma_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
