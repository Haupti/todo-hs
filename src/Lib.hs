module Lib where

import Classes (Presenter (..))
import Command.Command (Command (..), CommandParsingError (..), commandParsingFailedError, getCalledCommand, handleCommand)
import Time (localTime)
import Control.Monad (when)
import Data.Function ((&))
import Data.Maybe (isNothing)
import Logger (initialLogState, showLogs)
import Repository (getState, saveState)
import State (runState)
import Todo (TodoState (..), FinalStateProvider(..))

noStateError :: IO ()
noStateError = putStrLn "ERROR: loading state failed: state was nothing, should have been just the state"

stateFailedToSaveError :: IO ()
stateFailedToSaveError = putStrLn "ERROR: saving state failed: state was nothing, should have been just the state"

todo :: IO ()
todo = do
  commandParsingResult <- getCalledCommand
  savedState <- addCurrentTime getState

  logIfNoState savedState
  logIfError commandParsingResult

  case (commandParsingResult, savedState) of
    (Left command, Just presentState) -> do
      let (result, logs) = runState initialLogState (handleCommand command presentState)
      showLogs logs
      present result
      provideFinalState result & saveOrLogError
    _ -> return ()

saveOrLogError :: TodoState -> IO ()
saveOrLogError state = do
  saveResult <- saveState state
  when (isNothing saveResult) stateFailedToSaveError

logIfError :: Either Command CommandParsingError -> IO ()
logIfError (Right cmdErr) = commandParsingFailedError cmdErr
logIfError (Left _) = return ()

logIfNoState :: Maybe TodoState -> IO ()
logIfNoState Nothing = noStateError
logIfNoState (Just _) = return ()

addCurrentTime :: IO (Maybe TodoState) -> IO (Maybe TodoState)
addCurrentTime ioMayState = do
  mayState <- ioMayState
  time <- localTime
  return $ case mayState of 
    Nothing -> Nothing
    Just state -> Just $ state { currentDate = time }