module AddTodoCommand where

import Classes (FinalStateProvider (..), Presenter (..))
import Data.Function ((&))
import Logger (WithLogs)
import Repository (getState, saveState)
import Todo (Todo (..), TodoState (..))

data AddTodoCommandResult = AddTodoCommandResult
  { addedTodos :: [Todo],
    todoStateAfterAdding :: TodoState
  }

instance FinalStateProvider AddTodoCommandResult where
  finalTodoState = todoStateAfterAdding

instance Presenter AddTodoCommandResult where
  present (AddTodoCommandResult added _) = mapM_ (\td -> putStrLn $ show (orderNumber td) ++ todoDescription td) added

addTodos :: [String] -> TodoState -> WithLogs AddTodoCommandResult
addTodos descriptions currentState =
  pure $ addTodosToState descriptions currentState

addTodosToState :: [String] -> TodoState -> AddTodoCommandResult
addTodosToState descriptions state =
  let startNumber = map orderNumber (todos state) & maximum
      newTodos = mapToNewTodos descriptions (startNumber + 1)
   in AddTodoCommandResult {todoStateAfterAdding = state {todos = todos state ++ newTodos}, addedTodos = newTodos}

mapToNewTodos :: [String] -> Int -> [Todo]
mapToNewTodos (x : xs) number = Todo {orderNumber = number, todoDescription = x} : mapToNewTodos xs (number + 1)
mapToNewTodos [] _ = []
