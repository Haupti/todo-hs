module Lib where

import Command (Command (..), CommandParsingError (..), getCalledCommand)
import Data.Function ((&))
import Data.Functor (void, (<&>))
import Logger (Logs, WithLogs, addError, initialLogState, showLogs)
import Repository (getState, saveState)
import State (State, modify, runState)
import Todo (TodoState (..))

noStateErrorMsg :: String
noStateErrorMsg = "ERROR: state was nothing, should have been just the state"

todo :: IO ()
todo = do
  command <- getCalledCommand
  mayState <- getState
  let state = useJustOrElse mayState (handleCommand command) logErrorNoState
      (stateOrEmpty, logs) = runState initialLogState state
  showLogs logs
  useEither stateOrEmpty (void . saveState) return

logErrorNoState :: WithLogs (Either TodoState ())
logErrorNoState = do
  modify (addError noStateErrorMsg)
  return (Right ())

handleCommand :: Either Command CommandParsingError -> TodoState -> WithLogs (Either TodoState ())
handleCommand command state =
  return $ Right ()

useJustOrElse :: Maybe a -> (a -> b) -> b -> b
useJustOrElse (Just a) f _ = f a
useJustOrElse Nothing _ b = b

useEither :: Either a b -> (a -> c) -> (b -> c) -> c
useEither (Left a) fac _ = fac a
useEither (Right b) _ fbc = fbc b
