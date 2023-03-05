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

addTodos2 :: [String] -> TodoState -> WithLogs AddTodoCommandResult
addTodos2 descriptions currentState =
  pure $ addTodosToState2 descriptions currentState

addTodosToState2 :: [String] -> TodoState -> AddTodoCommandResult
addTodosToState2 descriptions state =
  let startNumber = map orderNumber (todos state) & maximum
      newTodos = mapToNewTodos descriptions (startNumber + 1)
   in AddTodoCommandResult {todoStateAfterAdding = state {todos = todos state ++ newTodos}, addedTodos = newTodos}

addTodos :: [String] -> IO (Maybe TodoState)
addTodos descriptions = do
  state <- getState
  let mayState = addTodosToState descriptions <$> state :: Maybe TodoState
  case mayState of
    Just justState -> saveState justState
    Nothing -> return Nothing

addTodosToState :: [String] -> TodoState -> TodoState
addTodosToState descriptions state =
  let startNumber = map orderNumber (todos state) & maximum
      newTodos = mapToNewTodos descriptions (startNumber + 1)
   in state {todos = todos state ++ newTodos}

mapToNewTodos :: [String] -> Int -> [Todo]
mapToNewTodos (x : xs) number = Todo {orderNumber = number, todoDescription = x} : mapToNewTodos xs (number + 1)
mapToNewTodos [] _ = []
