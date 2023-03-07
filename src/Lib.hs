module Lib where

import Command (Command (..), CommandParsingError (..), getCalledCommand, CommandResult(..))
import AddTodoCommand (addTodos)
import CheckTodoCommand (checkTodos)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Classes (FinalStateProvider(..), Presenter(..), PresentableProvider (providePresentable))
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
    MissingParamError cmd paramType -> putStrLn ("ERROR: " ++ cmd ++ " requires parameters of type: " ++ paramType)


todo :: IO ()
todo = do
  commandParsingResult <- getCalledCommand
  savedState <- getState

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

handleCommand :: Command -> TodoState -> WithLogs CommandResult
handleCommand cmd tds = case cmd of
  AddTodo todosRaw -> toResult <$> addTodos todosRaw tds
  CheckTodo todosToCheck -> toResult <$> checkTodos todosToCheck tds

toResult :: (PresentableProvider a, FinalStateProvider a) => a -> CommandResult
toResult a = CommandResult { presentable = providePresentable a, finalState = provideFinalState a}

logIfError :: Either Command CommandParsingError -> IO ()
logIfError (Right cmdErr) = commandParsingFailedError cmdErr
logIfError (Left _) = return ()

logIfNoState :: Maybe TodoState -> IO ()
logIfNoState Nothing = noStateError
logIfNoState (Just _) = return ()