module Lib where

import Command (CommandParsingError(..), Command(..), getCalledCommand)
import Data.Function ((&))
import Todo (TodoState(..))
import State (modify)
import Repository (getState, saveState)
import Logger (initialLogState, WithLogs, Logs, addError)

todo :: IO ()
todo = do
  command <- getCalledCommand
  mayState <- getState
  case mayState of
    Just state -> handleCommand command state
    Nothing -> do modify (addError "ERROR: state was nothing, should have been just the state") & return 
   & runState emptyInitialLog

handleCommand :: Either Command CommandParsingError -> TodoState -> WithLogs TodoState
handleCommand command state = 
  return state