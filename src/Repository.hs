module Repository where

import Todo (TodoState)
import Text.Read (readMaybe)
import Data.Functor ((<&>))

stateFile :: String
stateFile = "data.hsrn"

getState :: IO (Maybe TodoState)
getState = getStateFromFile stateFile

getStateFromFile :: FilePath -> IO (Maybe TodoState)
getStateFromFile filepath = 
  readFile filepath <&> (\content -> if not (null content) then (readMaybe content :: Maybe TodoState) else Nothing)

saveState :: TodoState -> IO (Maybe TodoState)
saveState = saveStateToFile stateFile

saveStateToFile :: FilePath -> TodoState -> IO (Maybe TodoState)
saveStateToFile filepath state = 
  writeFile filepath (show state)
    >> getStateFromFile filepath