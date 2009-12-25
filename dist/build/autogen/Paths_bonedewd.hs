module Paths_bonedewd (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/mae/.cabal/bin"
libdir     = "/home/mae/.cabal/lib/bonedewd-0.1/ghc-6.10.3"
datadir    = "/home/mae/.cabal/share/bonedewd-0.1"
libexecdir = "/home/mae/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "bonedewd_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "bonedewd_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "bonedewd_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "bonedewd_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
