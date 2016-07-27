module Paths_HaskellLatexFormatter (
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

bindir     = "C:\\Users\\Matthew\\Documents\\Haskell Programs\\HaskellLatexFormatter\\.stack-work\\install\\793afecd\\bin"
libdir     = "C:\\Users\\Matthew\\Documents\\Haskell Programs\\HaskellLatexFormatter\\.stack-work\\install\\793afecd\\lib\\x86_64-windows-ghc-7.10.3\\HaskellLatexFormatter-0.1.0.0-CCsxScH00Fh29Ph7qS09DD"
datadir    = "C:\\Users\\Matthew\\Documents\\Haskell Programs\\HaskellLatexFormatter\\.stack-work\\install\\793afecd\\share\\x86_64-windows-ghc-7.10.3\\HaskellLatexFormatter-0.1.0.0"
libexecdir = "C:\\Users\\Matthew\\Documents\\Haskell Programs\\HaskellLatexFormatter\\.stack-work\\install\\793afecd\\libexec"
sysconfdir = "C:\\Users\\Matthew\\Documents\\Haskell Programs\\HaskellLatexFormatter\\.stack-work\\install\\793afecd\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellLatexFormatter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellLatexFormatter_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HaskellLatexFormatter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellLatexFormatter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellLatexFormatter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
