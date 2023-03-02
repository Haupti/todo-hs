module CheckTodoCommand where

import Classes (FinalStateProvider (..), Presenter (..))
import Control.Monad (unless)
import Data.Function ((&))
import Data.List (elem, (\\))
import Logger (WithLogs, addInfo)
import Repository (getState, saveState)
import State (modify)
import Todo (DoneTodo (..), Todo (..), TodoState (..))

data CheckTodoCommandResult = CheckTodoCommandResult
  { checkedTodos :: [DoneTodo],
    todoStateAfterChecking :: TodoState
  }

instance FinalStateProvider CheckTodoCommandResult where
  finalTodoState = todoStateAfterChecking

instance Presenter CheckTodoCommandResult where
  present (CheckTodoCommandResult checked _) = mapM_ (putStrLn . doneDescription) checked

checkTodos :: [Int] -> IO (Maybe TodoState)
checkTodos orderNumbers = do
  state <- getState
  return $ checkTodosFromState orderNumbers <$> state

checkTodosFromState2 :: [Int] -> TodoState -> WithLogs CheckTodoCommandResult
checkTodosFromState2 nums state =
  let toCheck = getTodosByNumber nums state :: [Todo]
      asChecked = map (\(Todo _ description) -> DoneTodo {doneDescription = description}) toCheck
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

checkTodosFromState :: [Int] -> TodoState -> TodoState
checkTodosFromState nums state =
  let toCheck = getTodosByNumber nums state :: [Todo]
      asChecked = map (\(Todo _ description) -> DoneTodo {doneDescription = description}) toCheck
   in state
        { todos = todos state \\ toCheck,
          doneTodos = doneTodos state ++ asChecked
        }

getTodosByNumber :: [Int] -> TodoState -> [Todo]
getTodosByNumber orderNumbers state =
  todos state & filter (\(Todo orderNum _) -> orderNum `elem` orderNumbers)

notContained :: [Int] -> TodoState -> [Int]
notContained nums (TodoState tds _) = map orderNumber tds & (\\) nums
