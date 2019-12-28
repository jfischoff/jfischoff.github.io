{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_enjoyable_database_testing (
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

bindir     = "/Users/jonathanfischoff/prototypes/jfischoff.github.io/data/blog/enjoyable-database-testing/.stack-work/install/x86_64-osx/c56bc8d2d5eea439579bc0bb170b0d0d03f571e1dd84dff0e730408bac559620/8.8.1/bin"
libdir     = "/Users/jonathanfischoff/prototypes/jfischoff.github.io/data/blog/enjoyable-database-testing/.stack-work/install/x86_64-osx/c56bc8d2d5eea439579bc0bb170b0d0d03f571e1dd84dff0e730408bac559620/8.8.1/lib/x86_64-osx-ghc-8.8.1/enjoyable-database-testing-0.1.0.0-H4PXtkC1Dp1JutMdwo5gcY"
dynlibdir  = "/Users/jonathanfischoff/prototypes/jfischoff.github.io/data/blog/enjoyable-database-testing/.stack-work/install/x86_64-osx/c56bc8d2d5eea439579bc0bb170b0d0d03f571e1dd84dff0e730408bac559620/8.8.1/lib/x86_64-osx-ghc-8.8.1"
datadir    = "/Users/jonathanfischoff/prototypes/jfischoff.github.io/data/blog/enjoyable-database-testing/.stack-work/install/x86_64-osx/c56bc8d2d5eea439579bc0bb170b0d0d03f571e1dd84dff0e730408bac559620/8.8.1/share/x86_64-osx-ghc-8.8.1/enjoyable-database-testing-0.1.0.0"
libexecdir = "/Users/jonathanfischoff/prototypes/jfischoff.github.io/data/blog/enjoyable-database-testing/.stack-work/install/x86_64-osx/c56bc8d2d5eea439579bc0bb170b0d0d03f571e1dd84dff0e730408bac559620/8.8.1/libexec/x86_64-osx-ghc-8.8.1/enjoyable-database-testing-0.1.0.0"
sysconfdir = "/Users/jonathanfischoff/prototypes/jfischoff.github.io/data/blog/enjoyable-database-testing/.stack-work/install/x86_64-osx/c56bc8d2d5eea439579bc0bb170b0d0d03f571e1dd84dff0e730408bac559620/8.8.1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "enjoyable_database_testing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "enjoyable_database_testing_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "enjoyable_database_testing_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "enjoyable_database_testing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "enjoyable_database_testing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "enjoyable_database_testing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
