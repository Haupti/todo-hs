module Lib where

import Command (Command (..), CommandParsingError (..), getCalledCommand, CommandResult(..))
import Control.Monad (when)
import Data.Maybe (isNothing)
import Classes (FinalStateProvider(..), Presenter(..))
import Data.Function ((&))
import Logger (WithLogs, initialLogState, showLogs)
import Repository (getState, saveState)
import State (runState)
import Todo (TodoState (..))

noStateError :: IO ()
noStateError = putStrLn "ERROR: loading state failed: state was nothing, should have been just the state"

stateFailedToSaveError :: IO ()
stateFailedToSaveError = putStrLn "ERROR: saving state failed: state was nothing, should have been just the state"

commandParsingFailedError :: CommandParsingError -> IO ()
commandParsingFailedError cmdErr =
  case cmdErr of
    NoCommand -> putStrLn "ERROR: no command given"
    NoSuchCommand str -> putStrLn ("ERROR: no such command: " ++ str)


todo :: IO ()
todo = do
  commandParsingResult <- getCalledCommand
  savedState <- getState

  --checking preconditions
  logIfNoState savedState
  logIfError commandParsingResult
  case (commandParsingResult, savedState) of
    (Left command, Just presentState) -> do
        let (result, logs) = runState initialLogState (handleCommand command presentState)
        showLogs logs
        present result
        finalTodoState result & saveOrLogError
    _ -> return ()

  
saveOrLogError :: TodoState -> IO ()
saveOrLogError state = do
   saveResult <- saveState state
   when (isNothing saveResult) stateFailedToSaveError 

handleCommand :: Command -> TodoState -> WithLogs CommandResult
handleCommand cmd tds = return $ CommandResult tds

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight fn (Right b) = Right (fn b)
mapRight _ (Left a) = Left a

logIfError :: Either Command CommandParsingError -> IO ()
logIfError (Right cmdErr) = commandParsingFailedError cmdErr
logIfError (Left _) = return ()

logIfNoState :: Maybe TodoState -> IO ()
logIfNoState Nothing = noStateError
logIfNoState (Just _) = return ()