module Lib where

import Command (Command (..), CommandParsingError (..), getCalledCommand, CommandResult(..))
import Control.Monad (when)
import Data.Either (fromRight, fromLeft, isRight, isLeft)
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
  when (isNothing savedState) noStateError
  let presentState = fromJust savedState :: TodoState
  when (isRight commandParsingResult) (void $ mapRight commandParsingFailedError commandParsingResult)
  let command = fromLeft commandParsingResult

  let (result, logs) = runState initialLogState (handleCommand command presentState)
  showLogs logs
  present result
  finalTodoState result & saveState >>= \saveResult -> when (isNothing saveResult) stateFailedToSaveError 

  
handleCommand :: Command -> TodoState -> WithLogs CommandResult
handleCommand cmd tds = return $ CommandResult tds

useJustOrElse :: Maybe a -> (a -> b) -> b -> b
useJustOrElse (Just a) f _ = f a
useJustOrElse Nothing _ b = b

useEither :: Either a b -> (a -> c) -> (b -> c) -> c
useEither (Left a) fac _ = fac a
useEither (Right b) _ fbc = fbc b

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight fn (Right b) = Right (fn b)
mapRight _ (Left a) = Left a