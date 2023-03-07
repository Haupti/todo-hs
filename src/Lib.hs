module Lib where

import AddTodoCommand (addTodos)
import CheckTodoCommand (checkTodos)
import Classes (FinalStateProvider (..), Presenter (..))
import Command (Command (..), CommandParsingError (..), CommandResult (..), getCalledCommand)
import Control.Monad (when)
import Data.Function ((&))
import Data.Maybe (isNothing)
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
      finalTodoState result & saveOrLogError
    _ -> return ()

saveOrLogError :: TodoState -> IO ()
saveOrLogError state = do
  saveResult <- saveState state
  when (isNothing saveResult) stateFailedToSaveError

handleCommand cmd tds = return $
  CommandResult $
    case cmd of
      AddTodo strs -> addTodos strs tds
      CheckTodo ints -> checkTodos ints tds

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight fn (Right b) = Right (fn b)
mapRight _ (Left a) = Left a

logIfError :: Either Command CommandParsingError -> IO ()
logIfError (Right cmdErr) = commandParsingFailedError cmdErr
logIfError (Left _) = return ()

logIfNoState :: Maybe TodoState -> IO ()
logIfNoState Nothing = noStateError
logIfNoState (Just _) = return ()
