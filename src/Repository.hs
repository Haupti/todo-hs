module Repository where

import Config (FolderPath, filepath, folderpath)
import Control.Exception (IOException, SomeException, catch)
import Control.Monad (unless)
import Data.Functor ((<&>))
import System.Directory (createDirectoryIfMissing)
import Text.Read (readMaybe)
import Todo (TodoState, newTodoState)

getState :: IO (Maybe TodoState)
getState = do
  file <- filepath
  folder <- folderpath
  getStateFromFile file folder

getStateFromFile :: FilePath -> FolderPath -> IO (Maybe TodoState)
getStateFromFile file folder =
  readFileWithFallback file folder
    <&> ( \content ->
            if not (null content)
              then (readMaybe content :: Maybe TodoState)
              else Just newTodoState
        )

saveState :: TodoState -> IO (Maybe TodoState)
saveState todoState = do
  file <- filepath
  folder <- folderpath
  saveStateToFile file folder todoState

saveStateToFile :: FilePath -> FolderPath -> TodoState -> IO (Maybe TodoState)
saveStateToFile file folder state =
  writeFile file (show state)
    >> getStateFromFile file folder

readFileWithFallback :: FilePath -> FolderPath -> IO String
readFileWithFallback file folder = catch (readFile file) fallback
  where
    fallback :: SomeException -> IO String
    fallback _ = do
      unless (folder == "") (createDirectoryIfMissing True folder)
      writeFile file ""
      return ""
