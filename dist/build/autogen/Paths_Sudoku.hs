module Paths_Sudoku (
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
version = Version [0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sai/Desktop/Sudoku/.cabal-sandbox/bin"
libdir     = "/home/sai/Desktop/Sudoku/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/Sudoku-0.0.0"
datadir    = "/home/sai/Desktop/Sudoku/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/Sudoku-0.0.0"
libexecdir = "/home/sai/Desktop/Sudoku/.cabal-sandbox/libexec"
sysconfdir = "/home/sai/Desktop/Sudoku/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Sudoku_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Sudoku_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Sudoku_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Sudoku_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Sudoku_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
