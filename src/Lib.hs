module Lib (hasGitDirectory, gitStatus, gitAdd, gitAddAll, gitCommit, GitStatus(..)) where

import           Control.Monad.Extra  (anyM, void)
import           Data.Foldable.Extra  (notNull)
import           Data.Functor         ((<&>))
import           Data.List            (find, intercalate, isPrefixOf)
import           Data.List.Extra      (splitOn)
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
  runGitCommand' cwd' (args ++ ["--porcelain", "2"])

runGitCommandVoid :: FilePath -> [String] -> IO ()
runGitCommandVoid cwd' args = void $ runGitCommand' cwd' args



gitStatus :: FilePath -> IO GitStatus
gitStatus cwd' = runGitCommandPorcelain' cwd' ["status", "--branch"] <&> parseGitStatus

gitAdd :: FilePath -> String -> IO ()
gitAdd cwd' name' = runGitCommandVoid cwd' ["add", name']

gitAddAll :: FilePath -> IO ()
gitAddAll cwd' = runGitCommandVoid cwd' ["add", "."]

gitCommit :: FilePath -> String -> IO ()
gitCommit cwd' msg = runGitCommandVoid cwd' ["commit", "-m", msg]



data GitStatus = GitStatus { name       :: Maybe String
                           , upstream   :: Maybe String
                           , ahead      :: Maybe Int
                           , behind     :: Maybe Int
                           , fileStatus :: String
                           }
                deriving Show

parseGitStatus :: String -> GitStatus
parseGitStatus s = GitStatus { name = pName
                             , upstream = pUpstream
                             , ahead = pAhead
                             , behind = pBehind
                             , fileStatus = pFileStatus
                             }
    where
        getHeaders = map (drop 2) . filter (\s' -> take 2 s' == "# ")
        getFileStatus = intercalate "\n" . filter (\s' -> notNull s' && head s' /= '#')
        extractHeader key = fmap (drop $ length key + 3) . find (\s' -> ("# " ++ key) `isPrefixOf` s')

        pLines = splitOn "\n" s
        pName = extractHeader "branch.head" (getHeaders pLines)
        pUpstream = extractHeader "branch.upstream" (getHeaders pLines)
        ab = extractHeader "branch.ab" (getHeaders pLines)
        [pAhead, pBehind]= case ab of
          Just ab' -> map (read . drop 1) . take 2 . words $ ab'
          Nothing  -> [Nothing, Nothing]
        pFileStatus = getFileStatus pLines
