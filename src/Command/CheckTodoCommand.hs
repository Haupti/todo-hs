module Command.CheckTodoCommand where

import Classes (PresentableProvider (..))
import Control.Monad (unless)
import Data.Function ((&))
import Data.List ((\\))
import Logger (WithLogs, addInfo)
import State (modify)
import Todo (DoneTodo (..), FinalStateProvider (..), Todo (..), TodoState (..))

data CheckTodoCommandResult = CheckTodoCommandResult
  { checkedTodos :: [DoneTodo],
    todoStateAfterChecking :: TodoState
  }

instance FinalStateProvider CheckTodoCommandResult where
  provideFinalState = todoStateAfterChecking

instance PresentableProvider CheckTodoCommandResult where
  providePresentable (CheckTodoCommandResult checked _) = unlines $ map (\dn -> providePresentable (timeStamp dn) ++ " " ++ doneDescription dn) checked

checkTodos :: [Int] -> TodoState -> WithLogs CheckTodoCommandResult
checkTodos = checkTodosFromState

checkTodosFromState :: [Int] -> TodoState -> WithLogs CheckTodoCommandResult
checkTodosFromState nums state =
  let toCheck = getTodosByNumber nums state :: [Todo]
      asChecked = map (\(Todo _ description) -> DoneTodo {doneDescription = description, timeStamp = currentDate state}) toCheck
      notFound = notContained nums state
   in do
        unless (null notFound) (modify (addInfo $ "INFO: can not check todos: " ++ show notFound ++ ". not found."))
        return
          CheckTodoCommandResult
            { todoStateAfterChecking =
                state
                  { todos = todos state \\ toCheck,
                    doneTodos = doneTodos state ++ asChecked
                  },
              checkedTodos = asChecked
            }

getTodosByNumber :: [Int] -> TodoState -> [Todo]
getTodosByNumber orderNumbers state =
  todos state & filter (\(Todo orderNum _) -> orderNum `elem` orderNumbers)

notContained :: [Int] -> TodoState -> [Int]
notContained nums (TodoState tds _ _) = map orderNumber tds & (\\) nums
