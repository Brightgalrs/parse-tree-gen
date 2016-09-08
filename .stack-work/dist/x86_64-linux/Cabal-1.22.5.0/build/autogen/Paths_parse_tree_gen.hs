module Paths_parse_tree_gen (
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

bindir     = "/home/chaosrobie/Documents/parse-tree-gen/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/bin"
libdir     = "/home/chaosrobie/Documents/parse-tree-gen/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/lib/x86_64-linux-ghc-7.10.3/parse-tree-gen-0.1.0.0-Iaj1kxNSeaZ4PQxYHNqcNV"
datadir    = "/home/chaosrobie/Documents/parse-tree-gen/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/share/x86_64-linux-ghc-7.10.3/parse-tree-gen-0.1.0.0"
libexecdir = "/home/chaosrobie/Documents/parse-tree-gen/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/libexec"
sysconfdir = "/home/chaosrobie/Documents/parse-tree-gen/.stack-work/install/x86_64-linux/lts-6.15/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "parse_tree_gen_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "parse_tree_gen_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "parse_tree_gen_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parse_tree_gen_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "parse_tree_gen_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
