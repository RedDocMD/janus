module Main (main) where
import           Control.Monad       (forM)
import           Control.Monad.Extra (concatForM)
import           Data.Functor        ((<&>))
import           Data.List.Extra     (splitOn)
import           Data.Maybe          (fromMaybe)
import           Lib                 (hasGitDirectory)
import           System.Directory    (doesDirectoryExist, getDirectoryContents)
import           System.Environment  (lookupEnv)
import           System.FilePath     ((</>))

-- Musut be colon separated list of paths
searchEnvVar :: String
searchEnvVar = "JANUS_SEARCH"

excludeEnvVar :: String
excludeEnvVar = "JANUS_EXCLUDE"

getPaths :: String -> IO [FilePath]
getPaths var = lookupEnv var <&> (splitOn ":" . fromMaybe "")


getGitDirectories :: FilePath -> [FilePath] -> IO [FilePath]
getGitDirectories top exc = if top `elem` exc then return [] else do
    isGitDir <- hasGitDirectory top
    if isGitDir
        then return [top]
        else do
            names <- getDirectoryContents top
            let names' = filter (`notElem` [".", ".."]) names
            paths <- forM names' $ \name -> do
                let path = top </> name
                isDir <- doesDirectoryExist path
                if isDir
                    then getGitDirectories path exc
                    else return []
            return (concat paths)

getAllGitDirectories :: [FilePath] -> [FilePath] -> IO [FilePath]
getAllGitDirectories paths exc = concatForM paths (`getGitDirectories` exc)


main :: IO ()
main = do
    dirs <- getGitDirectories "/home/dknite/work" []
    putStrLn $ unwords dirs
