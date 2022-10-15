module Lib (hasGitDirectory) where

import           Control.Monad.Extra (anyM)
import           System.Directory    (listDirectory)
import           System.FilePath     (splitPath)
import           System.Posix.Files  (getFileStatus, isDirectory)

isGitDirectory :: FilePath -> IO Bool
isGitDirectory p = do
    fs <- getFileStatus p
    return (isDirectory fs && checkGit p)
    where checkGit pp = last (splitPath pp) == ".git"

hasGitDirectory :: FilePath -> IO Bool
hasGitDirectory p = listDirectory p >>= anyM isGitDirectory
