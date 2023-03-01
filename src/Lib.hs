module Lib where

import Command (Command (..), CommandParsingError (..), getCalledCommand, CommandResult(..))
import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Classes (FinalStateProvider(..), Presenter(..))
import Data.Function ((&))
import Logger (WithLogs, initialLogState, showLogs)
import Repository (getState, saveState)
import State (runState)
import Todo (TodoState (..), newTodoState)

noStateError :: IO ()
noStateError = putStrLn "ERROR: loading state failed: state was nothing, should have been just the state"

stateFailedToSaveError :: IO ()
stateFailedToSaveError = putStrLn "ERROR: saving state failed: state was nothing, should have been just the state"

todo :: IO ()
todo = do
  command <- getCalledCommand
  savedState <- getState
  when (isNothing savedState) noStateError
  let presentState = fromJust savedState :: TodoState
      (result, logs) = runState initialLogState (handleCommand command presentState)
  showLogs logs
  present result
  finalTodoState result & saveState >>= \saveResult -> when (isNothing saveResult) stateFailedToSaveError 


handleCommand :: Either Command CommandParsingError -> TodoState -> WithLogs CommandResult
handleCommand _ _ =
  return $ CommandResult newTodoState -- todo implement

useJustOrElse :: Maybe a -> (a -> b) -> b -> b
useJustOrElse (Just a) f _ = f a
useJustOrElse Nothing _ b = b

useEither :: Either a b -> (a -> c) -> (b -> c) -> c
useEither (Left a) fac _ = fac a
useEither (Right b) _ fbc = fbc b