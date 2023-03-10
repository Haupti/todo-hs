module Command.PurgeCommand where

import Classes (PresentableProvider (..))
import Control.Monad (unless, when)
import Data.Function ((&))
import Data.List ((\\))
import Logger (WithLogs, addInfo)
import State (modify)
import Todo (FinalStateProvider (..), Todo (..), TodoState (..))

newtype PurgeTodosCommandResult = PurgeTodosCommandResult TodoState

instance FinalStateProvider PurgeTodosCommandResult where
  provideFinalState (PurgeTodosCommandResult x) = x

instance PresentableProvider PurgeTodosCommandResult where
  providePresentable (PurgeTodosCommandResult _) = ""

purgeTodos :: [Int] -> TodoState -> WithLogs PurgeTodosCommandResult
purgeTodos nums state =
  let toPurge = getTodosByNumber nums state :: [Todo]
      notFound = notContained nums state
   in do
        unless (null notFound) (modify (addInfo $ "INFO: can not purge todos: " ++ show notFound ++ ". not found."))
        when (null toPurge) (modify (addInfo "INFO: nothing left to purge"))
        unless (null toPurge) (modify (addInfo $ "The following todos are purged from existence and history: " ++ show nums))
        return $
          PurgeTodosCommandResult
            state
              { todos = todos state \\ toPurge
              }

getTodosByNumber :: [Int] -> TodoState -> [Todo]
getTodosByNumber orderNumbers state =
  todos state & filter (\(Todo orderNum _) -> orderNum `elem` orderNumbers)

notContained :: [Int] -> TodoState -> [Int]
notContained nums (TodoState tds _ _) = map orderNumber tds & (\\) nums
