module Lib (hasGitDirectory, gitStatus, gitAdd, gitAddAll, gitCommit) where

import           Control.Monad        (void)
import           Control.Monad.Extra  (anyM)
import           System.Directory     (doesDirectoryExist, listDirectory)
import           System.Exit
import           System.FilePath      (splitPath)
import           System.Process.Extra (CreateProcess (cwd), proc,
                                       readCreateProcessWithExitCode)

isGitDirectory :: FilePath -> IO Bool
isGitDirectory p = do
  isDir <- doesDirectoryExist p
  return (isDir && checkGit p)
  where
    checkGit pp = last (splitPath pp) == ".git"

hasGitDirectory :: FilePath -> IO Bool
hasGitDirectory p = listDirectory p >>= anyM isGitDirectory



data GitCommandResult = GitCommandResult String ExitCode

gitPath :: String
gitPath = "/usr/bin/git"

runGitCommand :: FilePath -> [String] -> IO GitCommandResult
runGitCommand cwd' args = do
  let cp = (proc gitPath args) {cwd = Just cwd'}
  (code, out, _) <- readCreateProcessWithExitCode cp ""
  return $ GitCommandResult out code

runGitCommand' :: FilePath -> [String] -> IO String
runGitCommand' cwd' args = do
  GitCommandResult out code <- runGitCommand cwd' args
  case code of
    ExitSuccess -> return out
    ExitFailure val ->
      fail $
        "Failed to run \"" ++ renderGitCommand ++ "\": exit code " ++ show val
  where
    renderGitCommand = unwords (gitPath : args)

runGitCommandPorcelain' :: FilePath -> [String] -> IO String
runGitCommandPorcelain' cwd' args =
  runGitCommand' cwd' (args ++ ["--porcelain"])

runGitCommandVoid :: FilePath -> [String] -> IO ()
runGitCommandVoid cwd' args = void $ runGitCommand' cwd' args



gitStatus :: FilePath -> IO String
gitStatus cwd' = runGitCommandPorcelain' cwd' ["status"]

gitAdd :: FilePath -> String -> IO ()
gitAdd cwd' name = runGitCommandVoid cwd' ["add", name]

gitAddAll :: FilePath -> IO ()
gitAddAll cwd' = runGitCommandVoid cwd' ["add", "."]

gitCommit :: FilePath -> String -> IO ()
gitCommit cwd' msg = runGitCommandVoid cwd' ["commit", "-m", msg]
