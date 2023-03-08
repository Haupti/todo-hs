module Repository where

import Config (filepath)
import Control.Exception (IOException, catch)
import Data.Functor ((<&>))
import Text.Read (readMaybe)
import Todo (TodoState, newTodoState)

getState :: IO (Maybe TodoState)
getState = filepath >>= getStateFromFile

getStateFromFile :: FilePath -> IO (Maybe TodoState)
getStateFromFile filepath =
  readFileWithFallback filepath
    <&> ( \content ->
            if not (null content)
              then (readMaybe content :: Maybe TodoState)
              else Just newTodoState
        )

saveState :: TodoState -> IO (Maybe TodoState)
saveState todoState = do
  fp <- filepath
  saveStateToFile fp todoState

saveStateToFile :: FilePath -> TodoState -> IO (Maybe TodoState)
saveStateToFile filepath state =
  writeFile filepath (show state)
    >> getStateFromFile filepath

readFileWithFallback :: FilePath -> IO String
readFileWithFallback filepath = catch (readFile filepath) fallback
  where
    fallback :: IOException -> IO String
    fallback _ = writeFile filepath "" >> return ""
